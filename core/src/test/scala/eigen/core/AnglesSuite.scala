package eigen.core

class AnglesSuite extends munit.FunSuite:

  test("wrapDegrees keeps angles in [-180, 180)") {
    assertEqualsFloat(Angles.wrapDegrees(190f), -170f, 1e-4f)
    assertEqualsFloat(Angles.wrapDegrees(-190f), 170f, 1e-4f)
    assertEqualsFloat(Angles.wrapDegrees(180f), -180f, 1e-4f)
    assertEqualsFloat(Angles.wrapDegrees(720f + 10f), 10f, 1e-4f)
  }

  test("yawDelta takes the short way across the 360 seam") {
    assertEqualsFloat(Angles.yawDelta(179f, -179f), 2f, 1e-4f)
    assertEqualsFloat(Angles.yawDelta(-179f, 179f), -2f, 1e-4f)
    assertEqualsFloat(Angles.yawDelta(10f, 15f), 5f, 1e-4f)
  }
