#include <Rcpp.h>
#include <string>
#include <bitset>
using namespace Rcpp;

// [[Rcpp::export]]
int day15_part1(long value1, long value2){
  int same = 0;
  for(long i = 0; i < 40000000; i++){
    value1 = (16807 * value1) % 2147483647;
    value2 = (48271 * value2) % 2147483647;
    if(std::bitset<16>(value1) == std::bitset<16>(value2))
      same++;
  }
  return(same);
}

struct list_items {
  long value;
  list_items * next_item;
} ;

// [[Rcpp::export]]
int day15_part2(long value1, long value2){
  // Initialize end and start of pointers
  list_items * list1_start; list_items * list2_start;
  list1_start = (list_items*) malloc (1);
  list2_start = (list_items*) malloc (1);
  list_items * list1_end; list_items * list2_end; 
  list1_end = list1_start; list2_end = list2_start;
  long list1_length = 0; long list2_length = 0;
  // Loop and add elements to the lists
  while(list1_length <= 5000000 || list2_length <= 5000000){
    value1 = (16807 * value1) % 2147483647;
    value2 = (48271 * value2) % 2147483647;
    if(value1 % 4 == 0){
      (*list1_end).value = value1;
      list_items * next_item1 = (list_items*) malloc (1);
      (*next_item1).value = -1;
      (*list1_end).next_item = next_item1;
      list1_end = next_item1;
      list1_length++;
    }
    if(value2 % 8 == 0){
      (*list2_end).value = value2;
      list_items * next_item2 = (list_items*) malloc (1);
      (*next_item2).value = -1;
      (*list2_end).next_item = next_item2;
      list2_end = next_item2;
      list2_length++;
    }
  }
  // Compare the items
  int same = 0;
  while((*list1_start).value != -1 && (*list2_start).value != -1){
    if(std::bitset<16>((*list1_start).value) == std::bitset<16>((*list2_start).value)){
      same++;
    }
    list1_start = (*list1_start).next_item;
    list2_start = (*list2_start).next_item;
    tmp_i++;
  }
  return same;
}
