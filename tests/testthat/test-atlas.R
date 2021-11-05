library(ggseg)
library(ggseg3d)
library(ggplot2)

# ggseg ----
context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("kleist", package = "ggsegKleist")), 49)

  expect_error(brain_pal("kleist"), "not a valid")

  expect_true(all(brain_regions(kleist) %in% names(brain_pal("kleist", package = "ggsegKleist"))))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_brain_atlas(kleist))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = kleist),c("gg","ggplot"))

  expect_is(ggseg(atlas = kleist, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = kleist, mapping = aes(fill = region)) +
              scale_fill_brain("kleist", package = "ggsegKleist"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = kleist, mapping = aes(fill = region)) +
              scale_fill_brain("kleist", package = "ggsegKleist"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = kleist, mapping=aes(fill=region), adapt_scales = FALSE ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=kleist_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas(kleist_3d))

})
