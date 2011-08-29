require 'rails'

module JsOptimization
  extend ActiveSupport::Concern
  
  module StructExtensions
    module StructHasHtmlOpts
      def html_opts
        self[:html_opts] ||= {}
      end
    end
    
    module JsFileMethods
      def self.included(base)
        base.send(:include, StructHasHtmlOpts)
      end
      
      def sources
        self[:sources] ||= []
      end
      
      def to_args
        self.sources + [ self.html_opts ]
      end
    end
  
    module BasicStructToS
      def self.included(base); base.send(:include, StructHasHtmlOpts); end
      def to_s(opts={})
        output  = self.content.to_s.strip;
        output += ';' if output.rstrip.last != ';' && !opts[:without_semicolon]
        output
      end
    end
  end
  
  included do
    ## Structs
    JsRegular   = Struct.new(:content, :html_opts) { include JsOptimization::StructExtensions::BasicStructToS }
    JsExtra     = Struct.new(:content, :html_opts) { include JsOptimization::StructExtensions::BasicStructToS }
    JsOnload    = Struct.new(:content, :html_opts) { include JsOptimization::StructExtensions::BasicStructToS }
    JsFile      = Struct.new(:sources, :html_opts) { include JsOptimization::StructExtensions::JsFileMethods }
    JsBehavior  = Struct.new(:selector, :behavior, :html_opts) do
      include JsOptimization::StructExtensions::StructHasHtmlOpts
      def to_s
        %{'#{self.selector}' : #{self.behavior}}
      end
    end
  end
  
  module InstanceMethods
    def scripts_at_bottom?
      Object.const_defined?(:SCRIPTS_AT_BOTTOM) && SCRIPTS_AT_BOTTOM
    end  
    
    ALLOWED_JAVASCRIPT_TYPES =  [ :regulars, :extras, :top_extras, :bottom_extras, :very_top_extras, :very_bottom_extras, :onloads, :default_files, :files, :behaviors ].freeze
    
    ####################################################################################################################################
    ## Javascripts:                                                                                                                   ##
    ##    Normal, generic javascripts for some reason. Probably those that come in chunks, as opposed to inline javascripts           ##
    ## Extra Javascripts (top, bottom, nil):                                                                                          ##
    ##    From what I remember, the ones that come AFTER the normal printing of javascripts.                                          ##
    ##    Also, I think the Extra Javascripts were to be wrapped en masse by a <script>..</script> tag instead of individually.       ##
    ## Inline:                                                                                                                        ##
    ##    Those that are usually written <script type="text/javascript">Event.addBehavior({...});</script>                            ##
    ## Onloads:                                                                                                                       ##
    ##    window.browser = 'CustomBrowser'                                                                                            ##
    ## Javascript files:                                                                                                              ##
    ##    i.e. prototype.js                                                                                                           ##
    ## Behaviors:                                                                                                                     ##
    ##    "a#commentsToggler" : "CommentsToggleBehavior"                                                                              ##
    ####################################################################################################################################

    ############################################
    ## Javascripts Store Hash                 ##
    ############################################
    def javascripts; @_javascripts ||= Hash.new([]); end
    
    ALLOWED_JAVASCRIPT_TYPES.each do |js_type|
      module_eval(%{
        def js_#{js_type}
          javascripts[:#{js_type}]
        end
        }, 'js_optimization.rb', 63)
    end
    
    ############################################
    ## Manipulation of Javascripts Store Hash ##
    ## Used by "Add by type" section methods  ##
    ############################################
    private
    def append_javascripts(type, args)
      ALLOWED_JAVASCRIPT_TYPES.include?(type) or raise ArgumentError, "Invalid Deferred Javascript type"
      javascripts[type] += args
      return nil # Don't output anything to erb page right away. Defer output til bottom.
    end
    
    def prepend_javascripts(type, args)
      ALLOWED_JAVASCRIPT_TYPES.include?(type) or raise ArgumentError, "Invalid Deferred Javascript type"
      javascripts[type] = args + javascripts[type]
      return nil # Don't output anything to erb page right away. Defer output til bottom.
    end
    public
    
    ############################################
    ## Add by type                            ##
    ############################################
    # The `add_[type]` methods are what are publicly accessible from ERB.
    def add_javascripts(*args, &block)
      add_or_print_scripts(:regulars, *args, &block)
    end
    alias_method :add_scripts,      :add_javascripts # Use #add_javascripts internally, use #add_scripts from erb.
    alias_method :add_js_regulars,  :add_javascripts
    
    (ALLOWED_JAVASCRIPT_TYPES - [ :regulars ]).each do |js_type|
      module_eval(%{
        def add_js_#{js_type}(*args, &block)
          add_or_print_scripts(:#{js_type}, *args, &block)
        end
        }, 'js_optimization.rb', 111)
    end
    
    private
    # This method is what transforms the input arguments into their respective JsStruct objects.
    # This is the method that, based on `scripts_at_bottom?`, conditionally adds js to queue or prints js.
    def add_or_print_scripts(type, *args, &block)
      ALLOWED_JAVASCRIPT_TYPES.include?(type) or raise ArgumentError, "Invalid Deferred Javascript type"
      opts = args.extract_options!
      js_structs = case type.to_s
        when /files$/     then  JsFile.new(args, opts)
        when /behaviors/  then  args.map { |selector_and_behavior| JsBehavior.new(*selector_and_behavior, opts) }
        when /extras$/    then  block_given? ? JsExtra.new(capture(&block), opts) : args.map {|little_args| JsExtra.new(little_args, opts) }
        else
          structClass = "JsOptimization::Js#{type.to_s.classify}".constantize
          block_given? ? structClass.new(capture(&block), opts) : args.map { |little_args| structClass.new(little_args, opts) } 
      end
      should_add_to_queue = scripts_at_bottom? || [ :behaviors, :onloads ].include?(type)
      should_add_to_queue ? add_to_js_queue(type, js_structs) : print_specific_js_immediately!(type, js_structs)
    end
    
    # This assumes scripts_at_bottom? is true and args is an array composed of Js Structs.    
    def add_to_js_queue(type, *args) # This shouldn't worry about blocks. #add_or_print_scripts feeds a capture(&block)
      append_javascripts(type, args.flatten) # this method already returns nil.
    end
    public
    
    ############################################
    ## Printing of Javascripts by type        ##
    ############################################
    # This is only called when adding a js type in ERB but it can be printed immediately (assumes scripts_at_bottom? == false).
    private
    def print_specific_js_immediately!(type, *args)
      send(:"print_js_#{type}!", *args.flatten)
    end
    alias_method :print_javascripts!, :print_specific_js_immediately!
    
    def print_js_generic!(*structs)
      type = structs.first.is_a?(Symbol) ? structs.shift : :regulars
      return nil if (structs_to_print = structs.empty? ? send(:"js_#{type}") : structs).empty?
      javascript_tag_without_flip_flop(structs_to_print.map(&:to_s).join("\n"))
    end
    public
    
    [ :regulars, :extras, :top_extras, :bottom_extras, :very_top_extras, :very_bottom_extras ].each do |generic_type| # these all have a #to_s method.
      module_eval(%{
        def print_js_#{generic_type}!(*args)
          print_js_generic!(:#{generic_type}, *args)
        end
        }, 'js_optimization.rb', 160)
    end
    
    def print_js_extras_for(position, *args)
      send(:"print_js_#{position}_extras!", *args)
    end
    
    # For others, explicitly define below...
    # Usage:
    #   1. Called directly from ERB to denote where the deferred onloads should be printed.
    #   2. Called from #print_js_specific! which is called through #add_js_onloads which assumes !scripts_at_bottom?
    def print_js_onloads!(*structs) # Can't append comments to these.
      return nil if (structs_to_print = structs.empty? ? js_onloads : structs).empty?
      javascript_tag_without_flip_flop("Event.onReady(function(e) {" + structs_to_print.map(&:to_s).join("\n") + "});")
    end
    
    def print_js_files!(*structs)
      return nil if (structs_to_print = structs.empty? ? js_default_files + js_files : structs).empty?
      structs_to_print.map {|struct| javascript_include_tag(*struct.to_args.merge_options(:force => true)) }.join("\n")
    end
    
    def print_js_behaviors!(*structs)
      return nil if (structs_to_print = structs.empty? ? js_behaviors : structs).empty?
      javascript_tag_without_flip_flop("Event.addBehavior({" + structs_to_print.map(&:to_s).join(', ') + "});")
    end
    
    def output_scripts!(type, output)
      return raw(output) if Rails.env.production? # versus "live?" since it may be nice to see this in staging.
      raw(%{<!-- BEGIN #{type} -->\n} + output.to_s + %{\n<!-- END #{type} -->})
    end
    private :output_scripts!
    
    instance_methods.select { |meth_sym| meth_sym.to_s =~ /^print_js_.*!$/ }.each do |meth|
      method_name, type = meth.to_s.match(/^(print_js_(.*))!$/).to_a[1, 2]
      module_eval(%{
        def #{method_name}_with_html_safe!(*args)
          output_scripts!(:#{type}, #{method_name}_without_html_safe!(*args))
        end;
        alias_method_chain :#{method_name}!, :html_safe;
        }, 'js_optimization.rb', 197) if type != 'generic'
    end
    
    ##### HELPERS ##################
    # Just a check:
    def print_scripts_block?(pos=nil)
      send([ 'js', pos, 'extras' ].compact.join('_').intern).present?
    end
    
  end # End instance_methods
end

ActionView::Base.send(:include, JsOptimization)


module ActionView
  module Helpers
    JavaScriptHelper.module_eval do
      def javascript_tag_with_flip_flop(content_or_options_with_block=nil, html_options={}, &block)
        # Remove options not used but printed regardless by js_tag_wo_flip_flop
        position = (block_given? && content_or_options_with_block.is_a?(Hash) ? content_or_options_with_block : html_options).delete(:position)
        
        Object.const_defined?(:SCRIPTS_AT_BOTTOM) && SCRIPTS_AT_BOTTOM or return javascript_tag_without_flip_flop(content_or_options_with_block, html_options, &block)
        
        content = if block_given?
          html_options = content_or_options_with_block if content_or_options_with_block.is_a?(Hash)
          capture(&block)
        else
          content_or_options_with_block
        end
        
        if position
          send(:"add_js_#{position}_extras", content, html_options)
        else
          add_javascripts(content, html_options) # should return nil
        end
      end
      alias_method_chain :javascript_tag, :flip_flop
    end
  end
end

module ActionView
  module Helpers
    # Instead of alias_method_chaining javascript_include_tag, it's better to just do this which it calls at the end in all cases.
    AssetTagHelper.module_eval do
      def javascript_src_tag_with_flip_flop(source, options)
        if options.delete('force') or Object.const_defined?(:SCRIPTS_AT_BOTTOM) && !SCRIPTS_AT_BOTTOM
          javascript_src_tag_without_flip_flop(source, options)
        else
          shared_opts = { "type" => Mime::JS, "src" => path_to_javascript(source) }.merge(options)
          send(:"add_js_#{'default_' if @_virtual_path =~ /^shared\//}files", source, shared_opts)
        end
      end
      alias_method_chain :javascript_src_tag, :flip_flop
    end
  end
end