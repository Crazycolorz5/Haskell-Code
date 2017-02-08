//Really optimized QuickSort

#include <stdlib.h>
#include <time.h>
#include <algorithm>

extern "C" long* range(const unsigned long& len)
{
    long* lst = (long*) malloc(sizeof(long)*len);
    for(unsigned long i=0; i<len; i++)
    {
        lst[i] = i;
    }
    return lst;
}

extern "C" void shuffle(long* lst, const unsigned long& len)
//Does an in-place shuffle of lst with length len
{
  for(unsigned long i=0; i<len; i++)
  {
    unsigned long target = rand()%(len-i)+i;
    long temp = lst[target];
    lst[target] = lst[i];
    lst[i] = temp;
  }
}

extern "C" __attribute__((noinline)) void quickSort(long* lst, unsigned long& len)
{
    //unsigned int length_per_element_copy = length_per_element;
    void* sp;
    unsigned long pivot;
    unsigned long i=0; //left index, exclusive(0 is used for pivot)
    unsigned long j=len;
    unsigned long temp1;
    unsigned long temp2;//unused
    unsigned long l;
    unsigned long r;
    //asm routine optimization suggestions by Robert Sedgewick
    //www.csie.ntu.edu.tw/~b93076/p847-sedgewick.pdf
    
    //%0 is sp, %1 is pivot, %2 is i, %3 is j, %4 is temp1
    //%5 is temp2, %6 is l, %7 is r, %8 is lst
    asm __volatile__(
        "pushq %3\n\t"              //I'll need the length of the list to know when I'm done with insertionSort.
        "movq %%rsp, %0\n\t"        //Keep track of where the stack is so we know when we're done with recursive calls.
        "recurse:\n\t"
        "movq (%8, %2, 8), %1\n\t"  //load the pivot from initial position i(assume 0th element is pivot)
        "movq %3, %4\n\t"           //this section finds the middle element and compares it to the first
        "addq %2, %4\n\t"
        "shrq $0x1, %4\n\t"         //now, %4 is the index of the middle element.
        "movq (%8, %4, 8), %5\n\t"
        //"inc %2\n\t"
        "movq 8(%8, %2, 8), %6\n\t" //swap lst[i+1] and lst[i+mid]
        "movq %5, 8(%8, %2, 8)\n\t"
        "movq %6, (%8, %4, 8)\n\t"
        "movq -8(%8, %3, 8), %6\n\t" //rightmost element
        //at this point, %1 = [l], %2 = l, %3 = r, %5 = [l+1], %6 = [r]
        "cmp %5, %6\n\t"            //compares [l+1] and [r]
        "jle no_mid_right_swap\n\t"
        "movq %6, 8(%8, %2, 8)\n\t"
        "movq %5, -8(%8, %3, 8)\n\t"
        "movq %5, %7\n\t"
        "movq %6, %5\n\t"
        "movq %7, %6\n\t"
        "no_mid_right_swap:\n\t"
        "cmp %1, %6\n\t"            //compares [l] and [r]
        "jle no_left_right_swap\n\t"
        "movq %1, -8(%8, %3, 8)\n\t"
        "movq %6, (%8, %2, 8)\n\t"
        "mov %6, %1\n\t"            //we only need to worry about [l] now.
        "no_left_right_swap:\n\t"
        "cmp %5, %1\n\t"
        "jle no_left_mid_swap\n\t"
        "movq %1, 8(%8, %2, 8)\n\t"
        "movq %5, (%8, %2, 8)\n\t"
        "movq %5, %1\n\t"           //%1 is our pivot.
        "no_left_mid_swap:"
        "movq %2, %6\n\t"           //initial left index
        "inc %2\n\t"                //We already know this one is less than the pivot
        "movq %3, %7\n\t"           //initial right index
        "dec %3\n\t"                //We already know this one is greater than the pivot
        "jmp left_comparison_loop\n\t" //loop rotation
        "swap:\n\t"
        "movq (%8, %2, 8), %4\n\t"  //i<=j; we should swap them.
        "movq (%8, %3, 8), %5\n\t"
        "movq %4, (%8, %3, 8)\n\t"
        "movq %5, (%8, %2, 8)\n\t"  //They are now swapped.
        "left_comparison_loop:\n\t" //find something on the left that is greater than the pivot
        "inc %2\n\t"
        "cmp (%8, %2, 8), %1\n\t"   //Is our element greater?
        "jg left_comparison_loop\n\t"//no... keep searching
        "right_comparison_loop:\n\t" //find something on the right that is less than the pivot
        "dec %3\n\t"
        "cmp (%8, %3, 8), %1\n\t"   //Is it less than the pivot?
        "jl right_comparison_loop\n\t"//no... keep searching
        "cmp %2, %3\n\t"            //Okay, we found an inconsistency, but they(i and j) didn't cross while searching, did they?
        "jg swap\n\t"               //We haven't, so swap and keep looping
        "movq (%8, %3, 8), %5\n\t"  //now, swap the pivot with j. Load thing at j...
        "movq %1, (%8, %3, 8)\n\t"  //store pivot into index j...
        "movq %5, (%8, %6, 8)\n\t"  //now store the thing at j into where the pivot was.
        //now, we're ready for our recursive calls. First, determine the calls we need
        "pushq %3\n\t"              //store it before subtraction and comparison
        "subq %6, %3\n\t"           //j-=t1
        "cmp $0x0A, %3\n\t"         //limit for left-side's insertion sort
        "popq %3\n\t"
        "jle right_recurse_check\n\t" //We don't need to recurse on this side.
        "pushq %6\n\t"              //we do need to recurse on the left; push the left and right boundaries on.
        "pushq %3\n\t"
        "right_recurse_check:\n\t"
        "movq %7, %4\n\t"           //don't need t1 anymore. Load right endpoint
        "subq %2, %4\n\t"           //find length of right segment r_end - i
        "cmp $0x0A, %4\n\t"         //limit for insertion sort
        "jle recurse_or_exit\n\t"   //size(right)<=0x0A doesn't need recursing
        "pushq %2\n\t"              //we do need to recurse
        "pushq %7\n\t"
        "recurse_or_exit:\n\t"
        "cmp %0, %%rsp\n\t"         //is our stack of things to recurse upon empty?
        "je insertion_Sort\n\t"     //yes...
        "popq %3\n\t"               //no; pop things off and recurse.
        "popq %2\n\t"
        "jmp recurse\n\t"
        
        "insertion_Sort:\n\t"       //okay, now starting the insertionSort phase
        "popq %1\n\t"               //Get the length
        "dec %1\n\t"                //The algorithm is right-inclusive
        "movq $0x0, %2\n\t"         //Left bound
        "jmp first_run\n\t"         //Skip incrementation the first time
        "insertion_main_loop:\n\t"
        "inc %2\n\t"                //<= %2 is guaranteed sorted
        "first_run:\n\t"
        "cmp %2, %1\n\t"            //Have we sorted the whole list?
        "je end\n\t"                //yeah, we're done.
        "movq 8(%8, %2, 8), %6\n\t" //Take the element in question(the next one) into %6
        "cmp (%8, %2, 8), %6\n\t"   //Is it out of order?
        "jge insertion_main_loop\n\t"//Nope, move on
        "movq %2, %4\n\t"           //Yup, prepare for inner loop
        "no_insertion:\n\t"
        "movq (%8, %4, 8), %7\n\t"  //Move over an element.
        "movq %7, 8(%8, %4, 8)\n\t"
        "cmp $0x0, %4\n\t"          //Are we at the 0th element?
        "je at_zero\n\t"
        "insertion_inner_loop:\n\t"
        "dec %4\n\t"                //move left an element
        "cmp %6, (%8, %4, 8)\n\t"   //Dooooes it fit to the right of this?
        "jge no_insertion\n\t"      //if not keep looping
        "movq %6, 8(%8, %4, 8)\n\t" //Now we have to insert it. That's easy, we already moved stuff around so
        "jmp insertion_main_loop\n\t"
        "at_zero:\n\t"              //A rarely hit case (<= 10 per sort), so separated at a small efficiency cost due to pipelining
        "movq %6, (%8)\n\t"
        "jmp insertion_main_loop\n\t"
        "end:\n\t"
        :"=r"(sp), "=r"(pivot), "+r"(i), "+r"(j), "=r"(temp1), "=r"(temp2), "=r"(l), "=r"(r), "+r"(lst)
        :
        : "memory","cc");
}
