"""Contains common algorithms used throughout the project."""

class Algorithm:
    """Custom algorithms on Python data structures."""

    @staticmethod
    def pop_while(stack, predicate, action=None):
        """
        Pop all elements from a stack until a predicate returns False.
        
        An optional action is invoked after each iteration.

        Args:
            stack (list): The list to modify.
            predicate (function): The predicate to query.
            action (function): The action to invoke after each iteration.
        Returns:
            bool: True, if the stack is empty when returning. Otherwise, False.
        """
        isEmpty = True
        while stack:
            if predicate(stack[-1]):
                if not action is None:
                    action(stack[-1])
                stack.pop()
            else:
                isEmpty = False
                break
        return isEmpty

    @staticmethod
    def zip_all(a, b, predicate):
        """
        Compare two sequences for equality based on a predicate.

        Args:
            a: The first sequence.
            b: The second sequence.
            predicate (function): The predicate.
        Returns:
            bool: True, if the lists are equal. Otherwise False.
        """
        if len(a) != len(b):
            return False

        for i, e0 in enumerate(a):
            if not predicate(e0, b[i]):
                return False
        
        return True

    @staticmethod
    def join(delimiter, sequence):
        """
        Return a joined list of strings.

        Args:
            delimiter (str): The separator.
            sequence (iterable): The sequence to convert.
        Returns:
            str: The joined string values.
        """
        return delimiter.join(str(e) for e in sequence)

    @staticmethod
    def join_comma(sequence):
        """
        Return a comma-separated list of strings.

        Args:
            sequence (iterable): The list to convert.
        Returns:
            str: The comma-separated string values.
        """
        return Algorithm.join(", ", sequence)

    @staticmethod
    def join_dot(sequence):
        """
        Return a joined list of strings.

        Args:
            sequence (iterable): The sequence to convert.
        Returns:
            str: The joined string values.
        """
        return Algorithm.join(".", sequence)