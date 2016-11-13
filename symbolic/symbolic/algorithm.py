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

    @staticmethod
    def all_sequence(a, b, predicate):
        '''
        Compare two sequences for equality based on a predicate.

        Args:
            a: The first sequence.
            b: The second sequence.
            predicate (function): The predicate.
        Returns:
            bool: True, if the lists are equal. Otherwise False.
        '''
        for i, e0 in enumerate(a):
            if not predicate(e0, b[i]):
                return False
        
        return True

    @staticmethod
    def join(delimiter, l):
        '''
        Return a joined list of strings.

        Args:
            delimiter (str): The separator.
            l: The list to convert.
        Returns:
            str: The joined string values.
        '''
        return delimiter.join(str(e) for e in l)

    @staticmethod
    def join_comma(l):
        '''
        Return a comma-separated list of strings.

        Args:
            l: The list to convert.
        Returns:
            str: The comma-separated string values.
        '''
        return Algorithm.join(", ", l)

    @staticmethod
    def join_dot(l):
        '''
        Return a joined list of strings.

        Args:
            l: The list to convert.
        Returns:
            str: The joined string values.
        '''
        return Algorithm.join(".", l)