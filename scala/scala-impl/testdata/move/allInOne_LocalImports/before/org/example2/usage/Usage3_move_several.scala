package org.example2.usage

//
// SIMPLE
//
class Usage_Simple_1 {

  import org.example2.declaration.{U, V, W}

  val u: U = ???
  val v: V = ???
  val w: W = ???
}
class Usage_Simple_2 {

  import org.example2.declaration.{U, V, W, Y}

  val u: U = ???
  val v: V = ???
  val w: W = ???

  val y: Y = ???
}
class Usage_Simple_3 {

  import org.example2.declaration.{U, V, W, Y, Z}

  val u: U = ???
  val v: V = ???
  val w: W = ???

  val y: Y = ???
  val z: Z = ???
}

//
// SIMPLE SEPARATE IMPORTS
//
class Usage_Simple_SeparateImports_2 {

  import org.example2.declaration.{U, V, W}
  import org.example2.declaration.Y

  val u: U = ???
  val v: V = ???
  val w: W = ???

  val y: Y = ???
}

class Usage_Simple_SeparateImports_3 {

  import org.example2.declaration.{U, V, W}
  import org.example2.declaration.{Y, Z}

  val u: U = ???
  val v: V = ???
  val w: W = ???

  val y: Y = ???
  val z: Z = ???
}

//
// RENAMED
//
class Usage_Renamed_1 {

  import org.example2.declaration.{U => U_Renamed1, V, W => W_Renamed1}

  val u: U_Renamed1 = ???
  val v: V = ???
  val w: W_Renamed1 = ???
}
class Usage_Renamed_2 {

  import org.example2.declaration.{Y, U => U_Renamed2, V, W => W_Renamed2}

  val u: U_Renamed2 = ???
  val v: V = ???
  val w: W_Renamed2 = ???

  val y: Y = ???
}
class Usage_Renamed_3 {

  import org.example2.declaration.{Y, Z, U => U_Renamed3, V, W => W_Renamed3}

  val u: U_Renamed3 = ???
  val v: V = ???
  val w: W_Renamed3 = ???

  val y: Y = ???
  val z: Z = ???
}

//
// RENAMED HIDDEN
//
class Usage_Renamed_Hidden_1 {

  import org.example2.declaration.{U => _, V, W => _}

  val v: V = ???
}
class Usage_Renamed_Hidden_2 {

  import org.example2.declaration.{Y, U => _, V, W => _}

  val v: V = ???

  val y: Y = ???
}
class Usage_Renamed_Hidden_3 {

  import org.example2.declaration.{Y, Z, U => _, V, W => _}

  val v: V = ???

  val y: Y = ???
  val z: Z = ???
}

//
// RENAMED SEPARATE IMPORTS
//
class Usage_Renamed_SeparateImports_2 {

  import org.example2.declaration.{U => U_Renamed2, V, W => W_Renamed2}
  import org.example2.declaration.Y

  val u: U_Renamed2 = ???
  val v: V = ???
  val w: W_Renamed2 = ???

  val y: Y = ???
}
class Usage_Renamed_SeparateImports_3 {

  import org.example2.declaration.{U => U_Renamed3, V, W => W_Renamed3}
  import org.example2.declaration.{Y, Z}

  val u: U_Renamed3 = ???
  val v: V = ???
  val w: W_Renamed3 = ???

  val y: Y = ???
  val z: Z = ???
}

//
// RENAMED HIDDEN SEPARATE IMPORTS
//
class Usage_Renamed_Hidden_SeparateImports_2 {

  import org.example2.declaration.{U => _, V, W => _}
  import org.example2.declaration.Y

  val v: V = ???

  val y: Y = ???
}
class Usage_Renamed_Hidden_SeparateImports_3 {

  import org.example2.declaration.{U => _, V, W => _}
  import org.example2.declaration.{Y, Z}

  val v: V = ???

  val y: Y = ???
  val z: Z = ???
}