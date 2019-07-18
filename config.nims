const
  nimVersion = (major: NimMajor, minor: NimMinor, patch: NimPatch)

when nimVersion > (0, 20, 2):
  switch("styleCheck", "error")
