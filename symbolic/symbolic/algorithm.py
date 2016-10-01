class Algorithm:
    @staticmethod
    def pop_while(stack, func, action=None):
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
    def split_where(l, func):
        idx = -1
        for i, e in enumerate(l):
            if func(e):
                return l[:i], l[i:]
        return l, []