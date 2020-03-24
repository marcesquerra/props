lazy val scala213 = "2.13.1"
lazy val scala212 = "2.12.10"
lazy val scala211 = "2.11.12"
lazy val allScalaVersions = List(scala213, scala212, scala211)
lazy val nativeScalaVersions = List(scala211)


val sharedSettings = Seq(
    crossScalaVersions := {
      if (crossProjectPlatform.value == NativePlatform)
        nativeScalaVersions
      else
        allScalaVersions
    }
  , scalaVersion := scala211
)

lazy val root =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .in(file("."))
    .settings(sharedSettings)

    .jsSettings( // defined in sbt-scalajs-crossproject
      scalaJSUseMainModuleInitializer := true
     )
    .jvmSettings(/* ... */)

    // configure Scala-Native settings
    .nativeSettings( // defined in sbt-scala-native
      nativeLTO := "thin"
    )

addCommandAlias("buildNative", "rootNative/nativeLink")
addCommandAlias("buildFullNative", "+rootNative/nativeLink")

addCommandAlias("buildJS", "rootJS/compile")
addCommandAlias("buildFullJS", "+rootJS/compile")

addCommandAlias("buildJVM", "rootJVM/compile")
addCommandAlias("buildFullJVM", "+rootJVM/compile")

addCommandAlias("buildFull", "buildFullNative;buildFullJS;buildFullJVM")


addCommandAlias("runNative", "rootNative/run")
addCommandAlias("runFullNative", "+rootNative/run")

addCommandAlias("runJS", "rootJS/run")
addCommandAlias("runFullJS", "+rootJS/run")

addCommandAlias("runJVM", "rootJVM/run")
addCommandAlias("runFullJVM", "+rootJVM/run")

addCommandAlias("runFull", "runFullNative;runFullJS;runFullJVM")


addCommandAlias("testNative", "rootNative/test")
addCommandAlias("testFullNative", "+rootNative/test")

addCommandAlias("testJS", "rootJS/test")
addCommandAlias("testFullJS", "+rootJS/test")

addCommandAlias("testJVM", "rootJVM/test")
addCommandAlias("testFullJVM", "+rootJVM/test")

addCommandAlias("testFull", "testFullNative;testFullJS;testFullJVM")

addCommandAlias("loop", "~;+rootNative/nativeLink;+rootJS/compile;+rootJVM/compile;runFullNative;runFullJS;runFullJVM")
