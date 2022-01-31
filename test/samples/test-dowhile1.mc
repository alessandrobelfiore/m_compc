component EntryPoint provides App {
  def main() : int {
    var i : int;
    i = 5;
    do {
      print(i);
      i = i - 1;
    } while (i < 0);
    while (i < 0) {
      print(i);
      i = i - 1;
    }
    print(42);
    return 0;
  }
}