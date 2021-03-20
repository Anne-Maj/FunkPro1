package dk.cphbusiness.fun;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Program {

  public static void main(String[] args) {
    List<Integer> list = new ArrayList<>();
    list.add(7);
    list.add(8);
    list.add(9);

    var l2 = list.stream()
        .map(x -> 2*x).collect(Collectors.toList());

    for (int i : l2) {
      System.out.println(i);
      }
    }

  }
