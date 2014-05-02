def maxSubarray(arr,low,high)
   #
   # Divide-and-conquer algorithm for
   # finding a maximum-subarray
   #
   if high == low
      return [low, high, arr[low]]  # base case: only one element
   else
      # divide and conquer
      mid = ((low + high)/2).floor
      (leftlow,lefthigh,leftsum) = maxSubarray(arr,low,mid)
      (rightlow,righthigh,rightsum) = maxSubarray(arr,mid+1,high)
      (xlow,xhigh,xsum) = maxXingSubarray(arr,low,mid,high)
      # combine
      if leftsum >= rightsum and leftsum >= xsum
         return [leftlow,lefthigh,leftsum]
      elsif rightsum >= leftsum and rightsum >= xsum
         return [rightlow,righthigh,rightsum]
      else
         return [xlow,xhigh,xsum]
      end
   end
end

def maxXingSubarray(arr,low,mid,high)
   #
   # Subroutine of DC algorithm for finding a maximum-subarray
   #
   # Find a max-subarray of arr[i..mid]
   leftsum = -Float::INFINITY
   sum = 0
   for i in mid.downto(low)
      sum = sum + arr[i]
      if sum > leftsum
         leftsum = sum
         maxleft = i
      end
   end
   # Find a max-subarray of arr[mid+1..j]
   rightsum = -Float::INFINITY
   sum = 0
   for j in mid+1..high
      sum = sum + arr[j]
      if sum > rightsum
         rightsum = sum
         maxright = j
      end
   end
   # Return the indices i and j and the sum of the two subarrays
   return [maxleft,maxright,leftsum + rightsum]
end
