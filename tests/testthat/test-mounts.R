context("mounts")

test_that("detect_mount uses correct implementation", {
  ## Pretty heavy mocking here!
  mock_is_windows <- mockery::mock(TRUE, FALSE)
  mock_dmw <- mockery::mock()
  mock_dmu <- mockery::mock()

  mockery::stub(detect_mount, "is_windows", mock_is_windows)
  mockery::stub(detect_mount, "detect_mount_windows", mock_dmw)
  mockery::stub(detect_mount, "detect_mount_unix", mock_dmu)

  detect_mount()
  mockery::expect_called(mock_is_windows, 1)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 0)

  detect_mount()
  mockery::expect_called(mock_is_windows, 2)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 1)
})
