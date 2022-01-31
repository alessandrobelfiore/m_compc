component EntryPoint provides App {
  def main() : int {
    var i : int;
    var x : int;
    x = 0;
    i = 0;
    print(i++);
    print(i);
    print(i--);
    print(i);
    print(--i);
    print(i);
    print(++i);
    print(i);
    i = 10;
    x = i++;
    print(x);
    print(i);
    return 0;
  }
}
