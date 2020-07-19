name := "reverso"

version := "0.0.1"

scalaVersion := "2.13.3"

// --------
// Versions
// --------

val silencerVersion   = "1.7.0"
val catsVersion       = "2.1.1"
val catsEffectVersion = "2.2.0-RC1"
val fs2Version        = "2.4.2"

// ----------------
// Compiler Options
// ----------------

// Adapted from:
// https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions := Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                         // Specify character encoding used by source files.
  "-explaintypes",                 // Explain type errors in more detail.
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
  "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",        // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-dead-code",              // Warn when dead code is identified.
  "-Wextra-implicit",              // Warn when more than one implicit parameter section is defined.
  "-Wnumeric-widen",               // Warn when numerics are widened.
  "-Wunused:implicits",            // Warn if an implicit parameter is unused.
  "-Wunused:imports",              // Warn if unused imports
  "-Wunused:locals",               // Warn if a local definition is unused.
  "-Wunused:params",               // Warn if a value parameter is unused.
  "-Wunused:patvars",              // Warn if a variable bound in a pattern is unused.
  "-Wunused:privates",             // Warn if a private member is unused.
  "-Werror",                       // Fail the compilation if there are any warnings.
  s"-P:silencer:sourceRoots=${baseDirectory.value.getCanonicalPath}",
  "-P:silencer:pathFilters=target/scala-2\\.\\d+/(?:routes|twirl)/.*" // silence all warnings on autogenerated files
)

// ------------
// Dependencies
// ------------

resolvers += JCenterRepository

libraryDependencies ++= Seq(
  "org.typelevel"     %% "cats-core"       % catsVersion,
  "org.typelevel"     %% "cats-effect"     % catsEffectVersion,
  "org.typelevel"     %% "alleycats-core"  % catsVersion,
  "co.fs2"            %% "fs2-core"        % fs2Version,
  "org.choco-solver"   % "choco-solver"    % "4.10.2",
  "org.scalatest"     %% "scalatest"       % "3.1.0"         % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.0.0"       % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.14.0"        % Test,
  "com.github.ghik"   %% "silencer-lib"    % silencerVersion % Provided cross CrossVersion.full,
  compilerPlugin("org.typelevel"   %% "kind-projector"  % "0.10.3"),
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion cross CrossVersion.full)
)
