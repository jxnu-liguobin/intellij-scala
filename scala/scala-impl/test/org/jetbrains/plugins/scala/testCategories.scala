package org.jetbrains.plugins.scala

// tests marked with these categories will be run as a separate step


trait SlowTests

trait HighlightingTests

trait DebuggerTests

trait ScalacTests

trait TypecheckerTests

trait TestingSupportTests

trait UltimateTests

trait WorksheetEvaluationTests

/**
 * Will only be run at night.
 *
 * Especially, they will not be run to decide whether a branch should be merged or not.
 */
trait NightlyTests

/** Tests that may fail intermittently or depending on environment. 
 * Eg run locally but not on build server. */
trait FlakyTests
