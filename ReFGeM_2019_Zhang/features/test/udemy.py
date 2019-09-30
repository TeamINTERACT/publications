import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


class Solution:
    # def findTargetSumWays(self, nums, S):
    #     """
    #     :type nums: List[int]
    #     :type S: int
    #     :rtype: int
    #     """
    #     maxSum = sum(nums)
    #     if maxSum < S:
    #         return 0
    #     if maxSum == S:
    #         return 1
    #     nums.sort()
    #     index = [1] * len(nums)
    #     totWay = 0
    #
    #     def changeSymbol(changeIndex, curSum, curSymbolList):
    #         if curSymbolList[changeIndex] == -1:
    #             return 0
    #         else:
    #             newSum = curSum - 2 * nums[changeIndex]
    #             if newSum == S:
    #                 return 1
    #             elif newSum < S:
    #                 return 0
    #             else:
    #                 way = 0
    #                 newCurSymbolList = curSymbolList.copy()
    #
    #                 newCurSymbolList[changeIndex] = -1
    #                 for i in range(changeIndex+1, len(nums)):
    #                     partWay = changeSymbol(i, newSum, newCurSymbolList)
    #                     if not partWay:
    #                         break
    #                     else:
    #                         way += partWay
    #
    #                 for i in range(changeIndex + 1, len(nums)):
    #                     partWay = changeSymbol(i, curSum, curSymbolList.copy())
    #                     if not partWay:
    #                         break
    #                     else:
    #                         way += partWay
    #             return way
    #
    #     for j in range(len(nums)):
    #         totWay += changeSymbol(j, maxSum, index)
    #     return totWay


    def findTargetSumWays(self, nums, S):
        maxSum = sum(nums)
        if maxSum < S:
            return 0
        if maxSum == S:
            return 1
        self.wayCount = 0
        index = [1] * len(nums)
        nums.sort()
        def dfs(changeIndex, parentIndex, parentSum):
            if parentSum - 2 * nums[changeIndex] >= S:
                if parentSum - 2 * nums[changeIndex] == S:
                    self.wayCount += 1
                childIndex1 = parentIndex.copy()
                childIndex1[changeIndex] = 0
                for j in range(changeIndex+1, len(nums)):
                    dfs(j, childIndex1, parentSum - 2 * nums[changeIndex])
                    # dfs(j, parentIndex.copy(), parentSum)
        for i in range(len(nums)):
            dfs(i, index, maxSum)
        return self.wayCount






t = [1,5,4,8,17,30]
so = Solution()
print(so.findTargetSumWays(t,39))
