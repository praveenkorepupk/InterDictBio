# Similarly, if you have a global.R, you can call onStop() from there.
# ==== global.R ====
cat("Doing application setup\n")
onStop(function() {
  cat("Doing application cleanup\n")
})
# ==== end global.R ====