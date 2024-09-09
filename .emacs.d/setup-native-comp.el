;; faster packages??
(setq inhibit-automatic-native-compilation nil)
(setq native-comp-always-compile t)
(setq package-native-compile t)

;; Possible workaround for native comp failing to find -lemutls_w
;; As per https://github.com/d12frosted/homebrew-emacs-plus/issues/554#issuecomment-1564287827
;; TODO: Detect paths automatically so i don't have to touch this when homebrew upgrades
;; or on different macs (on apple silicon it's /opt/homebrew/opt/gcc/...)
;; I'll just add all for both m1 and x86 and assume missing directories are harmless
(setenv "LIBRARY_PATH"
	(mapconcat 'identity
	 '(
       "/opt/homebrew/lib/gcc/current"
       "/opt/homebrew/opt/libgccjit"
       )
     ":"))


