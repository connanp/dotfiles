;;; zig-mode.el --- sample major mode for editing Zig. -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq zig-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("comptime" "break" "inline" "default" "extern" "test" "else" "return" "orelse" "while" "switch" "defer" "catch" "export" "for" "if" "fn" "pub" "var" "try"))
            (x-types '("bool" "usize" "const" "void" "union" "enum" "error" "struct"))
            (x-constants '("usingnamespace" "unreachable" "noreturn" "true" "false"))
            (x-functions '("align" "callconv"))
            (x-builtins '("@addWithOverflow" "@alignCast" "@alignOf" "@ArgType" "@as"
                          "@asyncCall" "@atomicLoad" "@atomicRmw" "@atomicStore" "@bitCast"
                          "@bitOffsetOf" "@boolToInt" "@bitSizeOf" "@breakpoint" "@mulAdd"
                          "@byteSwap" "@bitReverse" "@byteOffsetOf" "@bytesToSlice" "@call"
                          "@cDefine" "@cImport" "@cInclude" "@clz" "@cmpxchgStrong"
                          "@cmpxchgWeak" "@compileError" "@compileLog" "@ctz" "@cUndef"
                          "@divExact" "@divFloor" "@divTrunc" "@embedFile" "@enumToInt"
                          "@errorName" "@errorReturnTrace" "@errorToInt" "@errSetCast" "@export"
                          "@fence" "@field" "@fieldParentPtr" "@floatCast" "@floatToInt"
                          "@frame" "@Frame" "@frameAddress" "@frameSize" "@hasDecl"
                          "@hasField" "@import" "@intCast" "@intToEnum" "@intToError"
                          "@intToFloat" "@intToPtr" "@IntType" "@memberCount" "@memberName"
                          "@memberType" "@memcpy" "@memset" "@mod" "@mulWithOverflow"
                          "@OpaqueType" "@panic" "@popCount" "@ptrCast" "@ptrToInt"
                          "@rem" "@returnAddress" "@setAlignStack" "@setCold" "@setEvalBranchQuota"
                          "@setFloatMode" "@setRuntimeSafety" "@shlExact" "@shlWithOverflow" "@shrExact"
                          "@shuffle" "@sizeOf" "@sliceToBytes" "@splat" "@sqrt"
                          "@sin" "@cos" "@exp" "@exp2" "@log"
                          "@log2" "@log10" "@fabs" "@floor" "@ceil"
                          "@trunc" "@round" "@subWithOverflow" "@tagName" "@TagType"
                          "@This" "@truncate" "@Type" "@typeId" "@typeInfo"
                          "@typeName" "@TypeOf" "@unionInit" "@Vector"))


            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-builtins-regexp (regexp-opt x-builtins 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-builtins-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))


;;;###autoload
(define-derived-mode zig-mode c-mode "zig mode"
  "Major mode for editing Zig"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((zig-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'zig-mode)

;;; zig-mode.el ends here
