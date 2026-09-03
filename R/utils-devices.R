# Opening a graphics device to measure or draw on has two traps, and theme61
# hits both in several places:
#
#   - dev.off() closes whatever is *current*, which need not be the device
#     this code opened - drawing can leave one of its own current.
#   - dev.off() then makes dev.next() current, which wraps around rather than
#     returning to the device the caller had.
#
# Get either wrong and text metrics come from an arbitrary device, so the same
# plot saves differently depending on session state. Pair t61_open_device()
# with t61_release_device() rather than calling svglite()/dev.off() directly.

#' Re-select `dev_num` as the current device, if it still exists -- text
#' layout can open and abandon devices of its own, leaving one of those
#' current instead, so drawing silently lands on the wrong device.
#' @noRd
t61_reclaim_device <- function(dev_num) {
  if (dev_num %in% grDevices::dev.list()) grDevices::dev.set(dev_num)
}

#' Open an svglite device, remembering which device the caller had current.
#' `...` is passed to svglite::svglite(). Pair with t61_release_device().
#' @noRd
t61_open_device <- function(...) {
  caller_dev <- grDevices::dev.cur()

  svglite::svglite(...)

  list(dev = grDevices::dev.cur(), caller = caller_dev)
}

#' Close a device opened by t61_open_device() and give the caller theirs back.
#' Idempotent, so it works both as an explicit call and as an on.exit() guard.
#' @noRd
t61_release_device <- function(device) {
  if (device$dev %in% grDevices::dev.list()) {
    grDevices::dev.set(device$dev)
    grDevices::dev.off()
  }

  t61_reclaim_device(device$caller)
}
