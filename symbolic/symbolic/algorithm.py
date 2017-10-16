import datetime

"""Contains common algorithms."""

class Algorithm:
    """Custom algorithms on Python data structures."""

    @staticmethod
    def dt_ms_string(start):
        """
        Return a pretty string for a time delta with a given start value.
        
        Args:
            start: The start date time.
        """
        seconds = int(round((datetime.datetime.now() - start).total_seconds()))
        minutes, seconds = divmod(seconds, 60)
        return "{:02d}m:{:02d}s".format(minutes, seconds)

    @staticmethod
    def pop_while(stack, predicate, action=None):
        """
        Pop all elements from a stack until the specified predicate returns False.
        
        An optional action is invoked after each iteration.

        Args:
            stack (list): The list to operate on.
            predicate (function): The predicate to query in each iteration.
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
        Compare two sequences for equality using a specified predicate.

        Args:
            a: The first sequence.
            b: The second sequence.
            predicate (function): The predicate.
        Returns:
            bool: True, if the sequences are equal. Otherwise False.
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
        Return a joined string from an input sequence and a delimiter.

        Args:
            delimiter (str): The delimiter (separator).
            sequence (iterable): The sequence to join.
        Returns:
            str: The joined string.
        """
        return delimiter.join(str(e) for e in sequence)

    @staticmethod
    def join_comma(sequence):
        """
        Return a joined string from an input sequence and a ','-separator.

        Args:
            sequence (iterable): The sequence to join.
        Returns:
            str: The joined string.
        """
        return Algorithm.join(", ", sequence)

    @staticmethod
    def join_dot(sequence):
        """
        Return a joined string from an input sequence and a '.'-separator.

        Args:
            sequence (iterable): The sequence to join.
        Returns:
            str: The joined string.
        """
        return Algorithm.join(".", sequence)