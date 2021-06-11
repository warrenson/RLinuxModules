context("RLinuxModules lifecycle .onLoad/.onUnload")

modulesHome    <- file.path(getwd(), "mock")
tmp_env        <-
  rep(NA, length(RLinuxModules:::.rlinuxmodules.names))
names(tmp_env) <- RLinuxModules:::.rlinuxmodules.names
empty_env      <- c(tmp_env,
                    'BASH_FUNC_module%%' = NA,
                    'BASH_FUNC_module()' = NA)
empty_env[['LOADEDMODULES']]          <- "git/2.10:perl/5.30"
empty_env[['LOADEDMODULES_modshare']] <- "git/2.10:1:perl/5.30:1"

test_that("package lifecycle", {
  unloadNamespace("RLinuxModules")

  withr::with_envvar(empty_env,
                     {
                       expect_true(is.na(version_set <- Sys.getenv("MODULE_VERSION", unset = NA)))
                       expect_true(is.na(version_set <- Sys.getenv("MODULE_VERSION_STACK", unset = NA)))
                       expect_true(requireNamespace("RLinuxModules"))
                       expect_equivalent(
                         names(RLinuxModules:::.rlinuxmodules),
                         RLinuxModules:::.rlinuxmodules.names,
                         label = "named list has names"
                       )

                       set_vars <-
                         RLinuxModules:::.rlinuxmodules[which(!is.na(RLinuxModules:::.rlinuxmodules))]
                       expect_equivalent(set_vars,
                                         c("git/2.10:perl/5.30", "git/2.10:1:perl/5.30:1"),
                                         label = "loaded as per environment")

                       # initialise and test variables, redundant overall, but required for later tests
                       withr::with_envvar(c("MODULE_VERSION"=NA, "MODULE_VERSION_STACK"=NA), {
                         # TODO: resolve why
                         # devtools::test() vs devtools::check()
                         # without this protection, MODULE_VERSION and MODULE_VERSION_STACK interfere below
                         moduleInit(modulesHome = modulesHome)
                         # I think this protection resets the version vars on leaving this block so
                         # post_init_vars does not include them.
                         # Have a feeling devtools::test() is wrong rather than devtools::check()
                       })
                       post_init_vars <-
                         Sys.getenv(RLinuxModules:::.rlinuxmodules.names, unset = NA)
                       var_names      <-
                         names(post_init_vars[which(!is.na(post_init_vars))])
                       expect_equivalent(
                         var_names,
                         c(
                           "LOADEDMODULES",
                           "LOADEDMODULES_modshare",
                           "MODULEPATH",
                           "MODULESHOME",
                           "MODULES_COLORS"
                         ),
                         label = "MODULE* variables set"
                       )

                       # unload and test variables are reset
                       unloadNamespace("RLinuxModules")

                       post_unload_vars <-
                         Sys.getenv(var_names, unset = NA)
                       expect_equivalent(
                         post_unload_vars,
                         c("git/2.10:perl/5.30", "git/2.10:1:perl/5.30:1", NA, NA, NA),
                         label = "reset"
                       )
                     })
})
