class Algorithm:
    '''Custom algorithms on Python data structures.'''
    @staticmethod
    def pop_while(stack, func, action=None):
        '''
        Pop all elements from a stack until a predicate returns
        false and invoke an optional action after each iteration.

        Returns:
            bool: True, if the stack is empty when returning. Otherwise, false.
        '''
        isEmpty = True
        while stack:
            if func(stack[-1]):
                if not action is None:
                    action(stack[-1])
                stack.pop()
            else:
                isEmpty = False
                break
        return isEmpty