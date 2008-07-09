module SymetrieCom
  module Acts #:nodoc:
    module NestedSet #:nodoc:
      def self.included(base)
        base.extend(SingletonMethods)
      end


      module SingletonMethods
        # Configuration options are:
        # * +parent_column+ - Column name for the parent/child foreign key (default: +parent_id+).
        # * +left_column+ - Column name for the left index (default: +lft+).
        # * +right_column+ - Column name for the right index (default: +rgt+). NOTE:
        #   Don't use +left+ and +right+, since these are reserved database words.
        def acts_as_nested_set(options = {})
          options = {
            :parent_column => 'parent_id',
            :left_column => 'lft',
            :right_column => 'rgt',
            :dependent => :delete_all, # or :destroy
          }.merge(options)

          write_inheritable_attribute(:acts_as_nested_set_options,
             { :parent_column  => (options[:parent_column] || 'parent_id'),
               :left_column    => (options[:left_column]   || 'lft'),
               :right_column   => (options[:right_column]  || 'rgt'),
               :class          => self # for single-table inheritance
              } )

          class_inheritable_reader :acts_as_nested_set_options

          # no bulk assignment
          attr_protected  acts_as_nested_set_options[:left_column].intern,
                          acts_as_nested_set_options[:right_column].intern,
                          acts_as_nested_set_options[:parent_column].intern
          # no assignment to structure fields

          module_eval <<-"end_eval", __FILE__, __LINE__
            def #{acts_as_nested_set_options[:left_column]}=(x)
              raise ActiveRecord::ActiveRecordError, "Unauthorized assignment to #{acts_as_nested_set_options[:left_column]}: it's an internal field handled by acts_as_nested_set code, use move_to_* methods instead."
            end
            def #{acts_as_nested_set_options[:right_column]}=(x)
              raise ActiveRecord::ActiveRecordError, "Unauthorized assignment to #{acts_as_nested_set_options[:right_column]}: it's an internal field handled by acts_as_nested_set code, use move_to_* methods instead."
            end
            def #{acts_as_nested_set_options[:parent_column]}=(x)
              raise ActiveRecord::ActiveRecordError, "Unauthorized assignment to #{acts_as_nested_set_options[:parent_column]}: it's an internal field handled by acts_as_nested_set code, use move_to_* methods instead."
            end
          end_eval


          include SymetrieCom::Acts::NestedSet::InstanceMethods
          extend  SymetrieCom::Acts::NestedSet::ClassMethods
          include SymetrieCom::Acts::NestedSet::Columns
          extend  SymetrieCom::Acts::NestedSet::Columns

          base_set_class.class_inheritable_accessor :acts_as_nested_set_scope_enabled
          base_set_class.acts_as_nested_set_scope_enabled = true

          before_create :set_left_right
          before_destroy :destroy_descendants

          #These need association methods to add or move records as necessary
          has_many :children, :class_name => self.name, :foreign_key => :parent_id
          belongs_to :parent, :class_name => self.name, :foreign_key => :parent_id

          named_scope :roots, :conditions => { :parent_id => nil }
          named_scope :leaves, :conditions => "#{left_col_name} + 1 = #{right_col_name}"
          #These should someday account for elem being an array
          named_scope :_ancestors, lambda {|elem| {:conditions=>"#{elem[left_col_name]} BETWEEN #{left_col_name} AND #{right_col_name}",:order => left_col_name} }
          named_scope :decendents, lambda {|elem| {:conditions=>"#{left_col_name} BETWEEN #{elem[left_col_name]} AND #{elem[right_col_name]}",:order => left_col_name} }
          named_scope :siblings, lambda{|elem| {:conditions=>{:parent_id=>elem.parent_id},:order => left_col_name} }
          named_scope :not_self, lambda{|elem| {:conditions=>["id != ?",elem.id], :order => left_col_name} }

        end
      end

      module ClassMethods

        # Checks the left/right indexes of all records,
        # returning the number of records checked. Throws ActiveRecord::ActiveRecordError if it finds a problem.
        def check_all
          total = 0
          transaction do
            # if there are virtual roots, only call check_full_tree on the first, because it will check other virtual roots in that tree.
            total = roots.inject(0) {|sum, r| sum + (r[r.left_col_name] == 1 ? r.check_full_tree : 0 )}
            raise ActiveRecord::ActiveRecordError, "Scope problems or nodes without a valid root" unless acts_as_nested_set_options[:class].count == total
          end
          return total
        end

        def root
          self.roots.first
        end

        #TODO Renumber based on parent_id

      end

      # This module provides instance methods for an enhanced acts_as_nested_set mixin. Please see the README for background information, examples, and tips on usage.
      module Columns
        # convenience methods to make the code more readable
        def left_col_name()#:nodoc:
          acts_as_nested_set_options[:left_column]
        end
        def right_col_name()#:nodoc:
          acts_as_nested_set_options[:right_column]
        end
        def parent_col_name()#:nodoc:
          acts_as_nested_set_options[:parent_column]
        end
        alias parent_column parent_col_name#:nodoc: Deprecated
        def base_set_class()#:nodoc:
          acts_as_nested_set_options[:class] # for single-table inheritance
        end
      end


      module InstanceMethods

        #should these not be added in the chain so they are not over-ridden
        # On creation, automatically add the new node to the right of all existing nodes in this tree.
        def set_left_right # already protected by a transaction
          maxright = base_set_class.maximum(right_col_name) || 0
          self[left_col_name] = maxright+1
          self[right_col_name] = maxright+2
        end

        # On destruction, delete all children and shift the lft/rgt values back to the left so the counts still work.
        def destroy_descendants # already protected by a transaction
          return if self[right_col_name].nil? || self[left_col_name].nil?
          self.reload # in case a concurrent move has altered the indexes
          dif = self[right_col_name] - self[left_col_name] + 1
          base_set_class.delete_all( "(#{left_col_name} BETWEEN #{self[left_col_name]} AND #{self[right_col_name]})" )
          base_set_class.update_all("#{left_col_name} = CASE \
                                      WHEN #{left_col_name} > #{self[right_col_name]} THEN (#{left_col_name} - #{dif}) \
                                      ELSE #{left_col_name} END, \
                                 #{right_col_name} = CASE \
                                      WHEN #{right_col_name} > #{self[right_col_name]} THEN (#{right_col_name} - #{dif} ) \
                                      ELSE #{right_col_name} END"
                                 )
        end

        # By default, records are compared and sorted using the left column.
        def <=>(x)
          self[left_col_name] <=> x[left_col_name]
        end

        #Returns true if this is a root node.
        def root?
          parent_id = self[parent_col_name]
          (parent_id == 0 || parent_id.nil?) && self[right_col_name] && self[left_col_name] && (self[right_col_name] > self[left_col_name])
        end

        #Returns true if this is a child node
        def child?
          parent_id = self[parent_col_name]
          !(parent_id == 0 || parent_id.nil?) && (self[left_col_name] > 1) && (self[right_col_name] > self[left_col_name])
        end

        #Returns true if we have no idea what this is
        def unknown?
          !root? && !child?
        end

        # Returns this record's root ancestor.
        def root
          # the BETWEEN clause is needed to ensure we get the right virtual root, if using those
          @root ||= with_scope(:conditions =>"#{self[left_col_name]} BETWEEN #{left_col_name} AND #{right_col_name}") {|| base_set_class.roots}
        end

        # Returns an array of all parents, starting with the root.
        def ancestors #doing this as shown we loose acces to Scope
          @ancestors ||= self_and_ancestors.not_self(self)
        end

        # Returns an array of all parents plus self, starting with the root.
        def self_and_ancestors
          @ancestors_self ||= base_set_class._ancestors(self)
        end

        # Returns all the children of this node's parent, except self.
        def siblings
          @siblings ||= self_and_siblings.not_self(self)
        end

        # Returns all the children of this node's parent, including self.
        def self_and_siblings
          @siblings_self ||= base_set_class.siblings(self)
        end

        # Returns the level of this object in the tree, root level being 0.
        def level
          return 0 if self[parent_col_name].nil?
          @level ||= base_set_class.ancestors(self).count - 1
        end

        # Returns the number of nested children of this object.
        def all_children_count
          return (self[right_col_name] - self[left_col_name] - 1)/2
        end

        # Returns itself and all nested children.
        def full_set
          @full_set ||= base_set_class.decendents(self)
        end

        # Returns all children and nested children.
         def all_children
          @all_children ||= full_set.not_self(self)
        end

        # Returns this record's terminal children (nodes without children).
        def leaves
          @leaves ||= full_set.leaves
        end

        def add_child(target)
          target.move_to_child_of(self)
        end
        # Move this node to the left of _target_ (you can pass an object or just an id).
        # Unsaved changes in either object will be lost. Raises ActiveRecord::ActiveRecordError if it encounters a problem.
        def move_to_left_of(target)
          self.move_to target, :left
        end

        # Move this node to the right of _target_ (you can pass an object or just an id).
        # Unsaved changes in either object will be lost. Raises ActiveRecord::ActiveRecordError if it encounters a problem.
        def move_to_right_of(target)
          self.move_to target, :right
        end

        # Make this node a child of _target_ (you can pass an object or just an id).
        # Unsaved changes in either object will be lost. Raises ActiveRecord::ActiveRecordError if it encounters a problem.
        def move_to_child_of(target)
          self.move_to target, :child
        end

        protected
        def move_to(target, position) #:nodoc:
          raise ActiveRecord::ActiveRecordError, "You cannot move a new node" if new_record?
          raise ActiveRecord::ActiveRecordError, "You cannot move a node if left or right is nil" unless self[left_col_name] && self[right_col_name]

          [ @leaves,@siblings,@ancestors,@level,@parent,
            @all_children,@full_set,@siblings_self,@ancestors_self].each {|i| i=nil}

          transaction do
            self.reload # the lft/rgt values could be stale (target is reloaded below)
            if target.is_a?(base_set_class)
              target.reload # could be stale
            else
              target = base_set_class.find(target) # load object if we were given an ID
            end

            if (target[left_col_name] >= self[left_col_name]) && (target[right_col_name] <= self[right_col_name])
              raise ActiveRecord::ActiveRecordError, "Impossible move, target node cannot be inside moved tree."
            end

            # the move: we just need to define two adjoining segments of the left/right index and swap their positions
            bound = case position
              when :child then target[right_col_name]
              when :left  then target[left_col_name]
              when :right then target[right_col_name] + 1
              else raise ActiveRecord::ActiveRecordError, "Position should be :child, :left or :right ('#{position}' received)."
            end

            if bound > self[right_col_name]
              bound = bound - 1
              other_bound = self[right_col_name] + 1
            else
              other_bound = self[left_col_name] - 1
            end

            return if bound == self[right_col_name] || bound == self[left_col_name] # there would be no change, and other_bound is now wrong anyway

            # we have defined the boundaries of two non-overlapping intervals,
            # so sorting puts both the intervals and their boundaries in order
            a, b, c, d = [self[left_col_name], self[right_col_name], bound, other_bound].sort

            # change nil to NULL for new parent
            if position == :child
              new_parent = target.id
            else
              new_parent = target[parent_col_name].nil? ? 'NULL' : target[parent_col_name]
            end

            base_set_class.update_all("\
              #{left_col_name} = CASE \
                WHEN #{left_col_name} BETWEEN #{a} AND #{b} THEN #{left_col_name} + #{d - b} \
                WHEN #{left_col_name} BETWEEN #{c} AND #{d} THEN #{left_col_name} + #{a - c} \
                ELSE #{left_col_name} END, \
              #{right_col_name} = CASE \
                WHEN #{right_col_name} BETWEEN #{a} AND #{b} THEN #{right_col_name} + #{d - b} \
                WHEN #{right_col_name} BETWEEN #{c} AND #{d} THEN #{right_col_name} + #{a - c} \
                ELSE #{right_col_name} END, \
              #{parent_col_name} = CASE \
                WHEN #{self.class.primary_key} = #{self.id} THEN #{new_parent} \
                ELSE #{parent_col_name} END"
            )
            self.reload
            target.reload
          end
        end

        def check #:nodoc:
          # performance improvements (3X or more for tables with lots of columns) by using :select to load just id, lft and rgt
          ## i don't use the scope condition here, because it shouldn't be needed
          my_children = base_set_class.find(:all, :conditions => "#{parent_col_name} = #{self.id}",
            :order => left_col_name, :select => "#{self.class.primary_key}, #{left_col_name}, #{right_col_name}")

          if my_children.empty?
            unless self[left_col_name] && self[right_col_name]
              raise ActiveRecord::ActiveRecordError, "#{self.class.name}##{self.id}.#{right_col_name} or #{left_col_name} is blank"
            end
            unless self[right_col_name] - self[left_col_name] == 1
              raise ActiveRecord::ActiveRecordError, "#{self.class.name}##{self.id}.#{right_col_name} should be 1 greater than #{left_col_name}"
            end
          else
            n = self[left_col_name]
            for c in (my_children) # the children come back ordered by lft
              unless c[left_col_name] && c[right_col_name]
                raise ActiveRecord::ActiveRecordError, "#{self.class.name}##{c.id}.#{right_col_name} or #{left_col_name} is blank"
              end
              unless c[left_col_name] == n + 1
                raise ActiveRecord::ActiveRecordError, "#{self.class.name}##{c.id}.#{left_col_name} should be 1 greater than #{n}"
              end
              c.check
              n = c[right_col_name]
            end
            unless self[right_col_name] == n + 1
              raise ActiveRecord::ActiveRecordError, "#{self.class.name}##{self.id}.#{right_col_name} should be 1 greater than #{n}"
            end
          end
        end #def Check

      end #InstanceMethods
    end #NestedSet
  end #Acts
end #SymetrieCom


