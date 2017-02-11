defmodule Liste do

    @doc """
    # *) Find the last element of a list.
    # Example:
    # ?- my_last(X,[a,b,c,d]).
    # X = d
    """
    def my_last([element|[]]) do
        element
    end

    def my_last([_|tail]) do
        my_last(tail)
    end

    @doc """
    #  (*) Find the last but one element of a list.
    # (zweitletztes Element, l'avant-dernier élément)
    """
    def last_but_one([element|[_|[]]]) do
        element
    end

    def last_but_one([_|tail]) do
        last_but_one(tail)
    end

    @doc """
    #  (*) Find the K'th element of a list.
    # The first element in the list is number 1.
    # Example:
    # ?- element_at(X,[a,b,c,d,e],3).
    # X = c
    """
    def element_at([], n) when n >= 0 do
        :error
    end

    def element_at([element|_], 0) do
        element
    end

    def element_at([_|tail], n) do
        element_at(tail, n-1)
    end

    @doc """
    # (*) Find the number of elements of a list.
    """
    def myLength(liste) do
        myLength(liste,0)
    end

    defp myLength([_|tail], n) do
        myLength(tail, n+1)
    end

    defp myLength([], n) do
        n
    end

    @doc """
    # (*) Reverse a list.
    """
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

    @doc """
    # (*) Find out whether a list is a palindrome.
    # A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
    """
    def pallindrome(liste) do
        liste == reverse(liste)
    end

    @doc """
    # (**) Flatten a nested list structure.
    # Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

    # Example:
    # ?- my_flatten([a, [b, [c, d], e]], X).
    # X = [a, b, c, d, e]
    """
    def flatten(liste) do
        flatten(liste, [])
    end

    defp flatten([element|[]], res) do
        case is_list(element) do
            false ->
                res ++ [element]
            true ->
                res ++ flatten(element)
        end
    end

    defp flatten([head|tail], res) do
        case is_list(head) do
            false -> 
                liste2 = [head] ++ flatten(tail)
                res = res ++ liste2    
            true ->
                liste2 = flatten(head) ++ flatten(tail)
                res = res ++ liste2
        end
    end

    @doc """
    # (**) Eliminate consecutive duplicates of list elements.
    # If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    # Example:
    iex> Liste.compress(Liste.compress([1,1,1,1,2,3,3,3,1,1,4,5,5,5,5])).
    [1, 2, 3, 1, 4, 5]
    """
    def compress(liste) do
        compress(liste,[])
    end

    defp compress([], res) do
        Enum.reverse(res)
    end
    
    defp compress([head|tail], res) do
        if length(res) > 0 and hd(res) == head do
            compress(tail, res)
        else
            compress(tail,[head] ++ res)
        end
    end

    @doc """
    (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:
    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
    """
    def pack(liste) do
        pack(liste, [])
    end

    defp pack([], res) do
        res
    end
    
    defp pack([head|tail], res) do
        [new_liste, new_res] = subpack(tail,[head])
        pack(new_liste, res ++ [new_res])
    end

    defp subpack([], res) do
        [[], res]
    end

    defp subpack([head|tail], res) do
        if head == hd(res) do
            subpack(tail, [head] ++ res)
        else
            [[head|tail],res]
        end
    end

    @doc """
    (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

    Example:
    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
    """
    def encode(liste) do
        subencode(pack(liste), [])
    end

    defp subencode([], res) do
        res
    end
    defp subencode([head|tail], res) do
        element = hd(head)
        size = length(head)
        subencode(tail, res ++ [[size,element]])
    end
end
