((magit-fetch "--prune")
 (magit-log:magit-log-mode "-n50" "--graph" "--color" "--decorate")
 (magit-pull "--rebase")
 (magit-revert "--edit"))
