# Парсер XML

## Что умеет

- Читать xml-данные из файла
- Строить по ним дерево
- Красиво отображать это дерево
- Ходить по нему

## Как поиграться

Запустить можно так: `cabal run xmltree -- example.xml`

После построения дерева можно пользоваться следующими командами в режиме REPL:
- `down i`: переключиться вниз на ребёнка текущего дерева с индексом `i` (в ноль-индексации)
- `up`: переключиться на родителя текущего дерева
- `attr name`: получить значение атрибута `name` корня текущего дерева
- `print`: распечатать текущее дерево
- `exit`: выйти из программы

Примеры вывода можно посмотреть [здесь](./output.txt)
