defmodule Liste do


    # *) Find the last element of a list.
    # Example:
    # ?- my_last(X,[a,b,c,d]).
    # X = d
    def my_last([element|[]]) do
        element
    end

    def my_last([_|tail]) do
        my_last(tail)
    end

    #  (*) Find the last but one element of a list.
    # (zweitletztes Element, l'avant-dernier élément)
    def last_but_one([element|[_|[]]]) do
        element
    end

    def last_but_one([_|tail]) do
        last_but_one(tail)
    end

    #  (*) Find the K'th element of a list.
    # The first element in the list is number 1.
    # Example:
    # ?- element_at(X,[a,b,c,d,e],3).
    # X = c
    def element_at([], n) when n >= 0 do
        :error
    end

    def element_at([element|_], 0) do
        element
    end

    def element_at([_|tail], n) do
        element_at(tail, n-1)
    end

    # (*) Find the number of elements of a list.
    def myLength(liste) do
        myLength(liste,0)
    end

    defp myLength([_|tail], n) do
        myLength(tail, n+1)
    end

    defp myLength([], n) do
        n
    end

    # (*) Reverse a list.
    def reverse(liste) do
        reverse(liste, [])
    end

    defp reverse([element|[]], liste) do
        liste ++ [element]
    end

    defp reverse([head|tail], liste) do
        liste2 = reverse(tail, liste)
        liste2 ++ [head]
    end

    # (*) Find out whether a list is a palindrome.
    # A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
    def pallindrome(liste) do
        liste == reverse(liste)
    end
end
