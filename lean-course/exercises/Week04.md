# Week 04 exercise: Trees and evaluation

**Format:** 90-minute exercise session. Work in `Templates/Week04.lean`; compare with `Course/Week04.lean` after attempting the tasks. The bonus task is optional.

## 1. Read and predict (20 minutes)

Draw the tree represented by `.node (.node .leaf 2 .leaf) 4 .leaf` and predict its size.

## 2. Program (45 minutes)

Complete `Tree.size` and `reverseAcc`. Write `Tree.height` and test it on an empty and a skewed tree.

## 3. Explain or prove (25 minutes)

Compare `reverseAcc` with a reverse based on appending at the end. Explain the likely cost difference and where strict evaluation matters.

## Bonus

Prove `Tree.size (Tree.map f t) = Tree.size t`.
