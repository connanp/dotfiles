;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.1:-VERS-TLS1.0")


(doom! :completion
       (company                         ; the ultimate code completion backend
        ;;+tng
        +childframe)
       ;; +auto)           ; as-you-type code completion
       ;;(helm             ; the *other* search engine for love and life
       ;; +fuzzy)          ; enable fuzzy search backend for helm
       ;;ido               ; the other *other* search engine...
       (ivy                             ; a search engine for love and life
        +fuzzy)                         ; enable fuzzy search backend for ivy

       :ui
       deft              ; notational velocity for Emacs
       doom                             ; what makes DOOM look the way it does
       doom-dashboard                   ; a nifty splash screen for Emacs
       doom-quit                        ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column       ; a `fill-column' indicator
       ligatures
       hl-todo                          ; highlight TODO/FIXME/NOTE tags
       modeline                         ; snazzy, Atom-inspired modeline, plus API
       nav-flash                        ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints                          ; display visual hints when editing in evil
       ;; treemacs                         ; a project drawer, like neotree but cooler
       (popup                           ; tame sudden yet inevitable temporary windows
        +all                            ; catch all popups that start with an asterix
        +defaults)                      ; default popup rules
       ;;(pretty-code      ; replace bits of code with pretty symbols
       ;; +iosevka)
       ;;tabs              ; an tab bar for Emacs
       ;;unicode           ; extended unicode support for various languages
       vc-gutter                        ; vcs diff in the fringe
       vi-tilde-fringe                  ; fringe tildes to mark beyond EOB
       window-select                    ; visually switch windows
       workspaces                       ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)               ; come to the dark side, we have cookies
       ;;(format +onsave)  ; automated prettiness
       file-templates                   ; auto-snippets for empty files
       format
       fold                             ; (nigh) universal code folding
       lispy                            ; vim for lisp, for people who dont like vim
       multiple-cursors                 ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       rotate-text                      ; cycle region at point between text candidates
       snippets
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired)                           ; making dired pretty [functional]
       ;; +ranger          ; bringing the goodness of ranger to dired
       ;; +icons                         ; colorful icons for dired-mode
       ;; electric          ; smarter, keyword-based electric-indent
       ;;ibuffer           ; interactive buffer management
       vc                               ; version-control and Emacs, sitting in a tree
       undo

       :term
       eshell                           ; a consistent, cross-platform shell (WIP)
       term                             ; terminals in Emacs
       ;;shell             ; a terminal REPL for Emacs
       vterm             ; another terminals in Emacs

       :checkers
       (syntax                        ; tasing you for every semicolon you forget
        +childframe)                    ; use childframes for error popups (Emacs 26+ only)
       ;; spell             ; tasing you for misspelling mispelling -- XXX broken with which-key--update when using z leader key
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       debugger                         ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       editorconfig                     ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup                          ; helps you navigate your code and documentation
        +devdocs                        ; ...on devdocs.io online
        +docsets)                       ; ...or in Dash docsets locally
       lsp
       ;;macos             ; MacOS-specific commands
       make                             ; run make tasks from Emacs
       magit                            ;
       pass                             ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy                          ; FIXME managing external services & code builders
       rgb                              ; creating color strings
       ;;tmux              ; an API for interacting with tmux
       ;; terraform
       ;;upload            ; map local to remote projects via ssh/ftp
       ;;wakatime

       :lang
       ;;agda              ; types of types of types of types...
       assembly                         ; assembly for fun or debugging
       (cc +lsp)               ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp                      ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data                             ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp                       ; drown in parentheses
       ess                              ; emacs speaks statistics
       ;;fsharp           ; ML stands for Microsoft's Language
       go                               ; the hipster dialect
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       json
       ;;julia             ; a better, faster MATLAB
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       (markdown
        +pandoc)                        ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;; (org                             ; organize your plain life in plain text
       ;;  +dragndrop                      ; file drag & drop support
       ;;  ;;+jupyter        ; ipython/jupyter support for babel
       ;;  +pandoc                         ; pandoc integration into org's exporter
       ;;  +present                        ; using Emacs for presentations
       ;;  +journal)
       perl                             ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python
        +ipython)                        ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest                             ; Emacs as a REST client
       (ruby)                         ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; rust                             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       sh                               ; she sells (ba|z)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       yaml
       web                              ; the tubes

       :email
       (mu4e)       ; WIP
       ;;notmuch             ; WIP
       ;;(wanderlust +gmail) ; WIP

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       calendar
       ;; irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;; literate

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +smartparens +evil-commands))


;; https://github.com/hlissner/doom-emacs/issues/4498
;; emacs --debug-init
;; benchmark-init/show-durations-tabuled
;; benchmark-init/show-durations-tree
(when doom-debug-p
  (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))
