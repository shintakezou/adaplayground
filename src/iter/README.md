Test with iterators
===================

This started simple and ended up as a mess when I've set my final goal
to the kind of iterator which allows you to write:

```ada
for E of L loop
  null;
end loop
```

It's wrong anyway because the type of `E` should have been `Natural`
instead of `Fibo_Type` (like if `Fibo_List` were a list of natural
numbers); in `fibotest.adb` this is hidden by overloading `Put_Line`.

The first and the second commented loops are those testing the
*fibonacci package* when it started as a very much cleaner
implementation of an iterator in the old Ada way.

The mess evolved from there.

Maybe I will clean up the code someday, or more likely I will keep it
and try a fresh start in the future.

The `for E of reverse L` isn't usable (there are missing parts).
