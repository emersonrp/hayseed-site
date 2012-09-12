;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Picking particular subsets of all defined items, for use by iterators & debugging.

(in-package :schema)

;;; Note that all of the stuff below has to be called _at runtime_, not at compile-time,
;;; since (among other reasons) it depends on the binding of *CURRENT-MICROWORLD-PACKAGE*.
;;; It is thus quite like the setup done at runtime by the microworld initialization functions
;;; (and must happen _after_ that setup).

;;; Hmm.  It occurs to me that the schema mechanism just assumes a linkage to the
;;; microworld anyway, in that it just assumes that *ITEM-ARRAY* will always hold
;;; items for the current microworld, etc.  So why didn't I just define ordinary
;;; global variables to hold the stuff below, confident that (if they're properly added
;;; to the initialization for the microworld) they'll just be set appropriately and
;;; that's that?  (Also means I don't have to have code that initializes iff not
;;; initialized---it can just do so unconditionally, like the normal initialization code
;;; for the microworld.)  Well, the answer is, I dunno.  Somehow I had a braino in the
;;; few minutes it took to write the code below, and I don't feel like fixing it
;;; now---this _is_ more general, anyway.
;;;
;;; Hell, if everything _else_ had been written this way, it would be possible to run
;;; more than one microworld at a time, without initialization, by just rebinding
;;; *CURRENT-MICROWORLD-PACKAGE* in between iterations!  (How's _that_ for a
;;; weird concept?)  [It'd be slightly slower, of course, to have to constantly do the
;;; lookup to figure out which array to access, so the speed hit would probably be
;;; unacceptable.]
;;;
;;; I guess what was concerning me is that, no matter _which_ particular items
;;; happen to be in *ITEM-ARRAY*, the schema mechanism still runs exactly the same
;;; code.  However, the code below is in support of domain-specific focus of
;;; attention, and _that_ means that _different_ code (it's domain-specific,
;;; remember) must run _inside_ the schema mechanism (to direct where the
;;; mechanism spends its time).  Hence, by doing things this way, I can cause the
;;; schema mechanism to run different code depending on which microworld is
;;; running, without having to recompile anything or make peculiar runtime
;;; dispatches all over the place.  [Yeah, yeah, _that_ was it!]

;;; Some terminology:  a "basic" class is one whose membership is derivable from
;;; some simple string match on the name of each item (e.g., items in class VF must
;;; have names _starting with_ (not just containing) "VF".  A "locale" is a set of
;;; items which have some spatial (or perhaps other) relationship to each other,
;;; not derivable from their names, and hence domain-specific.  (Strictly speaking,
;;; even basic classes are domain-specific, but a locale is probably more so.)

;;; The cache generally has both "names" and "numbers" stored in it (though it not
;;; mandatory that both be there for any given class).  A "name" is actually a _symbol_,
;;; _not_ a string, so we may do fast set operations on it and so forth.  [&&& At the
;;; moment, lookup of symbols in *ITEM-ARRAY* is slow, because they're not stored
;;; and we must use strings there.  I should fix this.]

;;; &&& I'll bet that several of the definitions below should be exported, but I'm not
;;; &&& going to do so until I need some of them elsewhere.

;;; While the schema mechanism itself doesn't need to know which microworld is running,
;;; the code below does, since it is a generic interface between the schema system and a
;;; given microworld's objects.  Rather than having to define the same code n times, once
;;; for each microworld (and _still_ having to come up with a hook to tie it to the schema
;;; mechanism), we instead assume a one-to-one mapping of microworlds to packages
;;; (already assumed by things such as DEF-DEFITEM et al), and rely on DEF-MICROWORLD-TOPLEVEL
;;; to bind this variable to the microworld's package, so we may get at microworld-specific
;;; information, such as its list of primitive items.
(defvar *CURRENT-MICROWORLD-PACKAGE* nil)	; Bound by the macro below.

;;; Called by DEF-MICROWORLD-TOPLEVEL.
(defmacro-exported WITH-CURRENT-MICROWORLD (symbol &body body)
  `(let ((*current-microworld-package* (symbol-package ,symbol)))
     ,@body))

;;; Things on this list get run just after initializing the microworld.
;;; Each individual package's microworld can push functions on this,
;;; and we'll only call those whose package is the same as that of
;;; *CURRENT-MICROWORLD-PACKAGE*.
(defvar *CURRENT-MICROWORLD-POST-MICROWORLD-INIT-FUNCTIONS* nil)

;;; Defines a function that should be run after initializing some microworld.
;;; The list is run in REVERSE ORDER, so that this macro can PUSH things on it
;;; in the order in which they are defined, yet have everything actually RUN
;;; in that order, too.
(defmacro-exported-definer DEF-POST-MICROWORLD-INIT (fn-name (&rest arglist) &body body)
  `(progn
     (defun ,fn-name (,@arglist)
       ,@body)
     (pushnew ',fn-name *current-microworld-post-microworld-init-functions*)))

;;; +++ Everything from here down assumes that WITH-CURRENT-MICROWORLD is
;;; +++ somewhere in the dynamic scope, so the information about which microworld
;;; +++ is in use does not have to be passed down in every caller (ain't special variables
;;; +++ wunnerful).

;;; These horrible variables are because I decided, very much after the fact,
;;; to use this same mechanism to group actions together.  So I need a way of
;;; temporarily rebinding the defaults to from items to actions when I need them.
;;; This is truly godawful, of course; I should just make some sort of caching
;;; class and use CLOS to keep track of all this mess.  But no time to fix it now.
;;; NOTE:  The first of these should NOT be interned here!  (The last two may be,
;;; since they're always going to be in SCHEMA:.)
(defparameter *ITEM-CLASS-CACHE-NAME* "*ITEM-CLASS-CACHE*")
(defparameter *PRIMITIVE-ITEMS-SYMBOL-NAME* "*PRIMITIVE-ITEMS*")
(defparameter *ITEM-ARRAY-SYMBOL-NAME* (intern "*ITEM-ARRAY*"))
(defparameter *ITEM-SYMBOL-NAME-TABLE-NAME* (intern "*PRIMITIVE-ITEM-SYMBOL-NAME-TABLE*"))

;;; Errs if somebody forgot to bind the relevant variable.
(defsubst CURRENT-MICROWORLD-PACKAGE ()
  (or *current-microworld-package*		; Assume that if it's bound, it's a package.  Seems safe.
      (error "~S is not bound to a package." '*current-microworld-package*)))

;;; Contains hardwired knowledge of what symbol is used by DEF-DEFITEM.
(defsubst CURRENT-MICROWORLD-PRIMITIVE-ITEMS-SYMBOL ()
  (intern *primitive-items-symbol-name*	; &&& It's a pity that INTERN-SOFT & INTERN-LOCAL-SOFT are Genera-isms.
	  (current-microworld-package)))

;;; Note that it is NOT SAFE to bash this list!  It's not a copy!
(defsubst CURRENT-MICROWORLD-PRIMITIVE-ITEMS ()
  (symbol-value (current-microworld-primitive-items-symbol)))

;;; A local cache, one per microworld.
(defsubst ITEM-CLASS-CACHE ()
  (let ((symbol (intern *item-class-cache-name* (current-microworld-package))))
    (or (and (boundp symbol)
	     (symbol-value symbol))		; Assume that, if it's bound, it's bound to a hash table.  Seems safe.
	(set symbol (make-hash-table)))))

(defsubst CLEAR-ITEM-CLASS-CACHE ()		; Debugging only.
  (clrhash (item-class-cache)))			; Return the table.

(defsubst CLASS-IN-CACHE? (key)
  (gethash key (item-class-cache)))

;;; Calls FILTER-FN with args of ITEM (a symbol from some *PRIMITIVE-ITEMS*) and
;;; FILTER-ARGS, and associates the items which survive with the key.  Returns the
;;; new list of items.  Note that items are stuck on the given list in the order in
;;; which they occur in *PRIMITIVE-ITEMS*.  Since that list is usually "backwards"
;;; (because new elements are pushed on by DEFITEM), this means that individual
;;; lists here are "backwards" in the same way.  I can't decide if this is a bug or a
;;; feature; it'd be trivial to add a REVERSE below.
(defsubst CACHE-ITEM-CLASS (key filter-fn &rest filter-args)
  ;; Unconditionally bashes KEY's entry in the table.  Useful for forced recomputation.
  (setf (gethash key (item-class-cache))
	(remove-if-not #'(lambda (item)
			   (apply filter-fn item filter-args))
		       (current-microworld-primitive-items))))

;;; Like the above, but if an entry already exists for the key, returns it instead of
;;; computing & storing a new one.  Thus, if there's already such an entry, won't
;;; run the FILTER-FN.  This does _not_ have "basic" in its name, because the
;;; FILTER-FN is completely general, and might be implementing a locale or something.
(defsubst MAYBE-CACHE-ITEM-CLASS (key filter-fn &rest filter-args)
  (or (class-in-cache? key)
      (apply #'cache-item-class key filter-fn filter-args)))

;;; Only returns the items, optionally erring if the class hasn't been created yet.
;;; Never creates the class, hence doesn't take any FILTER-FN.
;;; If NONEXISTENT-OKAY is non-NIL, and the class doesn't exist, we return it.
;;; Note that this allows you to tell (without multiple values) if the class didn't
;;; exist, but forces you to check either for a returned value that's EQ to what
;;; you supplied for NONEXISTENT-OKAY, or at least for whether the value was a
;;; list (not necessarily a cons) or not.
(defsubst ITEMS-OF-CLASS (key &optional nonexistent-okay)
  (or (gethash key (item-class-cache))
      (or nonexistent-okay			; Not UNLESS!
	  (error "No items of class ~S in ~S."
		 key
		 (current-microworld-primitive-items-symbol)))))

;;; This one returns either NIL or the items in the class (we assume that the fact
;;; that an empty class and NIL are indistinguishable doesn't matter---ah, for #!false).
;;; This is suitable for use in OR and so forth.  (This is really just a renamed version
;;; of CLASS-IN-CACHE?, but I wound up reinventing the wheel here...)
(defsubst ITEMS-OF-CLASS-OR-NIL (key)
  (class-in-cache? key))

;;; Used in lots of places below.
(defsubst CLASS-NAME-TO-NUMBERED-CLASS-NAME (class-symbol)
  (intern (string-append (symbol-name class-symbol) "-NUMBERS")
	  (symbol-package class-symbol)))

;;; These two are for classes of item numbers.  Same idea as above.
(defsubst ITEMS-OF-NUMBERED-CLASS (key &optional nonexistent-okay)
  (items-of-class (class-name-to-numbered-class-name key) nonexistent-okay))

(defsubst ITEMS-OF-NUMBERED-CLASS-OR-NIL (key)
  (items-of-class-or-nil (class-name-to-numbered-class-name key)))

;;; Another name for the above.
(defsubst NUMBERED-CLASS-IN-CACHE? (key)
  (items-of-numbered-class-or-nil key))

;;; I often need both the items and their numbers.  Make this easy.  Explodes if
;;; either was unavailable (e.g., somebody should have cached 'em by now), unless
;;; NONEXISTENT-OKAY was supplied, in which case we handle as above.
(defsubst ITEM-NAMES-AND-NUMBERS-OF-CLASS (key &optional nonexistent-okay)
  (let ((item-names   (items-of-class          key nonexistent-okay))
	(item-numbers (items-of-numbered-class key nonexistent-okay)))
    (values item-names item-numbers)))

;;; Note that this evaluates KEY, but not ITEM-NAMES or ITEM-NUMBERS.  Because of
;;; this, putting KEY after them doesn't really violate the apparent left-to-right
;;; evaluation order.  (I did this so that calls aren't ugly:  "...(names numbers 'vf)"
;;; is prettier than "...('vf names numbers)", and yet I don't want to have to make
;;; another version that doesn't evaluate its key arg, even though I suspect it will
;;; almost always be a compile-time constant.)
(defmacro WITH-ITEM-NAMES-AND-NUMBERS-OF-CLASS ((item-names item-numbers key
						     &optional nonexistent-okay)
						&body body)
  `(multiple-value-bind (,item-names ,item-numbers)
       (item-names-and-numbers-of-class ,key ,nonexistent-okay)
     ,@body))

;;; Turns a string into a symbol, since cache elements that aren't numbers should be symbols.
;;; Not used by the basic classes, which walk the list of primitive items (a list of symbols)
;;; with a filter function.  Instead, used by things which generate arbitrary lists without
;;; doing so by filtering the list of primitive items, but then might have a bunch o' names
;;; that want to get turned back into symbols.  Yeah, intern is slow, but probably just as
;;; slow as searching the list, given that doing so would have to keep getting pnames and
;;; doing string comparisons etc.  The right solution is for items to have their own symbols
;;; known, as well as their pnames.
(defsubst ITEM-STRING-TO-ITEM-SYMBOL (string)
  (intern-up string (current-microworld-package)))

(defsubst ITEM-STRINGS-TO-ITEM-SYMBOLS (strings)
  (mapcar #'item-string-to-item-symbol strings))

;;; Allows associating arbitrary data (not just a list of item symbols) in the cache.
;;; Intended for lists of item numbers (not symbols), but could be used for anything.
;;; Unconditionally bashes the entry for KEY, a la CACHE-ITEM-CLASS.
(defsubst CACHE-ARBITRARY (key data)
  (setf (gethash key (item-class-cache))
	data))

;;; This is to CACHE-ARBITRARY as MAYBE-CACHE-ITEM-CLASS is to CACHE-ITEM-CLASS.
(defsubst MAYBE-CACHE-ARBITRARY (key data)
  (or (class-in-cache? key)
      (cache-arbitrary key data)))

;;; +++ There are lots of decision points below where it's not clear whether to
;;; +++ unconditionally smash existing cache entries or not.  For the most part, I've
;;; +++ written the code for speed (e.g., reuse cache entries---else why cache?), in the
;;; +++ hopes that anything that will _really_ invalidate some cache (presumably,
;;; +++ recompiling some critical piece of code) will also take care to just clear the hash
;;; +++ table and be done with it.  (Currently, starting a run clears the entire cache, for
;;; +++ instance.)  Because of this, most of the routine names start with "MAYBE-", to
;;; +++ emphasize this.  Routines that start with the comment "toplevel" associated with
;;; +++ them generally try to find the info already cached, and regenerate it if not, and
;;; +++ are usually only defined for things that are fairly toplevel and might get all called
;;; +++ from one initialization routine.  This also applies in EYEHAND-ITERATORS as well as
;;; +++ in this file.

;;; A convenience function for building classes whose key is simply related to the
;;; name of the primitive items in the class.  SYMBOL names the class.  Note that this
;;; finds items whose names _start with_ the pname of SYMBOL, _not_ those which
;;; _contain_ the pname; this prevents us from finding FOVF when SYMBOL is 'VF.
(defsubst MAYBE-CACHE-ITEM-BASIC-CLASS-NAMED (symbol)
  (let* ((name (symbol-name symbol))		; This is just so it doesn't have to be recomputed on every call to the filter function.
	 (length (length name)))		; Ditto.
    (maybe-cache-item-class
      symbol
      #'(lambda (item partial-name partial-name-length)
	  (string-equal partial-name (symbol-name item)
			:end2 partial-name-length))
      name
      length)))

;;; Takes either a symbol or a string.  If it got a symbol, coerces it into a string.
;;; (Yes, I know that CL tends to coerce naked symbols into strings for most functions
;;; that want strings, but this does the coercion once, up front, rather than potentially
;;; inside a loop or something.)
(defmacro SYMBOL-TO-NAME (name)
  `(when (symbolp ,name)
     (setf ,name (symbol-name ,name))))

;;; As of 27 Mar 94, the version of this function that has to search is obsolete,
;;; finally.  We now store item symbols explicitly in a parallel hashtable to *ITEM-ARRAY*.
;;;
;;; Finds a given, specifically-named primitive item, and returns its item number.
(defsubst NUMBER-OF-ITEM-NAMED (symbol)
  (or (gethash symbol (symbol-value *item-symbol-name-table-name*))
      (error "Didn't find number for item named ~S." symbol)))

;;; Old version:
; (defsubst NUMBER-OF-ITEM-NAMED (name)
;   ;; *sigh* It turns out that *ITEM-ARRAY* stores the pname of the item, and its
;   ;; function objects, but _not_ its symbol!  This probably made sense at one time
;   ;; as some sort of efficiency hack, but it kills us now, and forces us to use
;   ;; STRING-EQUAL instead of EQL for comparison when searching the array.
;   ;; [We can't ask the function object for its name and compare that to a symbol,
;   ;; because FUNCTION-NAME isn't ANSI---it's in SYS:.]  &&& We should probably
;   ;; change *ITEM-ARRAY* to store a symbol as well as the pname.
;   (symbol-to-name name)
;   (position name (symbol-value *item-array-symbol-name*)
; 	    :test #'string-equal
; 	    :key #'item-print-name))

(defsubst NUMBERS-OF-ITEMS-NAMED (names)	; Convenience function.
  (mapcar #'number-of-item-named names))

(defsubst ITEM-NUMBERS-TO-NAMES (numbers)	; Intended for debugging, but who knows?
  (mapcar #'(lambda (number)
	      (item-print-name (get-item number)))
	  numbers))

;;; Caches the numbers for some basic class of items.  Does _not_ have "basic"
;;; in its name, because the class could have been created in any arbitrary way,
;;; and hence may be a locale.  Returns the list of numbers.
(defun MAYBE-CACHE-NUMBERS-FOR-CLASS-NAMED (class-symbol)
  (maybe-cache-arbitrary
    (class-name-to-numbered-class-name
      class-symbol)
    (numbers-of-items-named
      (items-of-class class-symbol))))		; E.g., what we just computed in CACHE-ITEM-CLASS-NAMED above.

;;; Caches both the names and object numbers for a given basic class.
(defun MAYBE-CACHE-ITEM-BASIC-CLASS-AND-NUMBERS-NAMED (class-symbol)
  (maybe-cache-item-basic-class-named  class-symbol)
  (maybe-cache-numbers-for-class-named class-symbol))

;;; End of file.
