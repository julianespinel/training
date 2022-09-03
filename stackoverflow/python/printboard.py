from enum import Enum


class Shape(str, Enum):
    empty = '-'
    cross = 'X'
    circle = 'O'


TOP_BORDER = '-------------'
SIDE_BORDER = '|'


class Board:
    def __init__(self, rows: list[list[Shape]]) -> None:
        self.rows = rows

    def print(self) -> None:
        print(TOP_BORDER)
        for row in self.rows:
            print(self._row_to_str(row))
            print(TOP_BORDER)

    def _row_to_str(self, row: list[Shape]) -> str:
        row_contents = []
        for shape in row:
            row_contents.append(f' {shape} ')
        row_str = SIDE_BORDER.join(row_contents)
        return f'{SIDE_BORDER}{row_str}{SIDE_BORDER}'


if __name__ == '__main__':
    row = [Shape.empty] * 3
    rows = [row] * 3
    board = Board(rows)
    board.print()
