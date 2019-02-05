namespace Test1
open Xunit

module ``Compiler tester`` =
  [<Fact>]
  let ``Test Compiler``() =
    Assert.Equal(4, 2+2)
