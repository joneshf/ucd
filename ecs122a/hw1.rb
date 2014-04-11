def selection_sort array
    len = array.length
    for i in 0...len
        smallest = i
        for j in i...len
            if array[j] < array[smallest]
                smallest = j
            end
        end
        array[i], array[smallest] = array[smallest], array[i]
    end
    array
end

def naive_min_max array
    minimum, maximum = array[0], array[0]
    for element in array[1..array.size]
        if element < minimum
            minimum = element
        end
        if element > maximum
            maximum = element
        end
    end
    [minimum, maximum]
end

# def efficient_min_max array
#     if array.length == 2
#         if array[0] < array[1]
#             [array[0], array[1]]
#         end
#     else
#         mid = array.length / 2
#         left1, right1 = [array[0...mid], array[mid...array.length]]
#         # left1, right1 = divide array
#         left2 = efficient_min_max left1
#         right2 = efficient_min_max right1
#         conquer left2, right2
#     end
# end

# def conquer a1, a2
#     min1, max1 = a1
#     min2, max2 = a2
#     if min1 < min2
#         abs_min = min1
#     else
#         abs_min = min2
#     end
#     if max1 > max2
#         abs_max = max1
#     else
#         abs_max = max2
#     end
#     [abs_min, abs_max]
# end

# def divide array
#     mid = array.length / 2
#     [array[0...mid], array[mid...array.length]]
# end

def efficient_min_max array
    if array.length == 2
        if array[0] < array[1]
            [array[0], array[1]]
        else
            [array[1], array[0]]
        end
    else
        # Divide
        mid = array.length / 2
        min1, max1 = efficient_min_max array[0...mid]
        min2, max2 = efficient_min_max array[mid...array.length]
        # Conquer
        if min1 < min2
            abs_min = min1
        else
            abs_min = min2
        end
        if max1 > max2
            abs_max = max1
        else
            abs_max = max2
        end
        [abs_min, abs_max]
    end
end
