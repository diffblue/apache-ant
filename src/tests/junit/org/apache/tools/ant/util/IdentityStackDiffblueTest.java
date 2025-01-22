package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Stack;
import org.junit.Test;

public class IdentityStackDiffblueTest {
  /**
   * Test {@link IdentityStack#getInstance(Stack)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance_given42_whenStackAdd42_thenReturnStack() {
    // Arrange
    Stack<Object> s = new Stack<>();
    s.add("42");

    // Act
    IdentityStack<Object> actualInstance = IdentityStack.getInstance(s);

    // Assert
    assertEquals(s, actualInstance);
  }

  /**
   * Test {@link IdentityStack#getInstance(Stack)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance_given42_whenStackAdd42_thenReturnStack2() {
    // Arrange
    Stack<Object> s = new Stack<>();
    s.add("42");
    s.add("42");

    // Act
    IdentityStack<Object> actualInstance = IdentityStack.getInstance(s);

    // Assert
    assertEquals(s, actualInstance);
  }

  /**
   * Test {@link IdentityStack#getInstance(Stack)}.
   * <ul>
   *   <li>When {@link IdentityStack#IdentityStack()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance_whenIdentityStack_thenReturnEmpty() {
    // Arrange and Act
    IdentityStack<Object> actualInstance = IdentityStack.getInstance(new IdentityStack<>());

    // Assert
    assertTrue(actualInstance.isEmpty());
  }

  /**
   * Test {@link IdentityStack#getInstance(Stack)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance_whenNull_thenReturnEmpty() {
    // Arrange and Act
    IdentityStack<Object> actualInstance = IdentityStack.getInstance(null);

    // Assert
    assertTrue(actualInstance.isEmpty());
  }

  /**
   * Test {@link IdentityStack#getInstance(Stack)}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then return {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance_whenStack_thenReturnStack() {
    // Arrange
    Stack<Object> s = new Stack<>();

    // Act
    IdentityStack<Object> actualInstance = IdentityStack.getInstance(s);

    // Assert
    assertEquals(s, actualInstance);
  }

  /**
   * Test {@link IdentityStack#IdentityStack()}.
   * <p>
   * Method under test: {@link IdentityStack#IdentityStack()}
   */
  @Test
  public void testNewIdentityStack() {
    // Arrange and Act
    IdentityStack<Object> actualObjectList = new IdentityStack<>();

    // Assert
    assertTrue(actualObjectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#IdentityStack(Object)}.
   * <p>
   * Method under test: {@link IdentityStack#IdentityStack(Object)}
   */
  @Test
  public void testNewIdentityStack2() {
    // Arrange and Act
    IdentityStack<Object> actualObjectList = new IdentityStack<>("42");

    // Assert
    assertEquals(1, actualObjectList.size());
    assertEquals("42", actualObjectList.get(0));
  }

  /**
   * Test {@link IdentityStack#contains(Object)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains_givenIdentityStackAdd42_thenReturnTrue() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.contains("42"));
  }

  /**
   * Test {@link IdentityStack#contains(Object)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add two.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains_givenIdentityStackAddTwo_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add(2);

    // Act and Assert
    assertFalse(objectList.contains("42"));
  }

  /**
   * Test {@link IdentityStack#contains(Object)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains_givenIdentityStack_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.contains("42"));
  }

  /**
   * Test {@link IdentityStack#indexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOfWithObjectInt_givenIdentityStackAdd42_whenOne_thenReturnOne() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add("42");

    // Act and Assert
    assertEquals(1, objectList.indexOf("42", 1));
  }

  /**
   * Test {@link IdentityStack#indexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add two.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOfWithObjectInt_givenIdentityStackAddTwo_thenReturnRetry_forever() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add(2);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, objectList.indexOf("42", 1));
  }

  /**
   * Test {@link IdentityStack#indexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOfWithObjectInt_givenIdentityStack_whenOne_thenReturnRetry_forever() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, objectList.indexOf("42", 1));
  }

  /**
   * Test {@link IdentityStack#lastIndexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOfWithObjectInt_givenIdentityStackAdd42_whenOne_thenReturnOne() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add("42");

    // Act and Assert
    assertEquals(1, objectList.lastIndexOf("42", 1));
  }

  /**
   * Test {@link IdentityStack#lastIndexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add two.</li>
   *   <li>When one.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOfWithObjectInt_givenIdentityStackAddTwo_whenOne_thenReturnZero() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add(2);

    // Act and Assert
    assertEquals(0, objectList.lastIndexOf("42", 1));
  }

  /**
   * Test {@link IdentityStack#lastIndexOf(Object, int)} with {@code Object}, {@code int}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOfWithObjectInt_whenRetry_forever_thenReturnRetry_forever() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, objectList.lastIndexOf("42", Retryable.RETRY_FOREVER));
  }

  /**
   * Test {@link IdentityStack#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_given42_whenArrayListAdd42_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertFalse(objectList.removeAll(c));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_given42_whenArrayListAdd42_thenReturnFalse2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertFalse(objectList.removeAll(c));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenIdentityStackAdd42_whenArrayListAdd42_thenReturnTrue() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualRemoveAllResult = objectList.removeAll(c);

    // Assert
    assertTrue(objectList.isEmpty());
    assertTrue(actualRemoveAllResult);
  }

  /**
   * Test {@link IdentityStack#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenIdentityStack_whenArrayList_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.removeAll(new ArrayList<>()));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When {@link HashSet#HashSet()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenIdentityStack_whenHashSet_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.removeAll(new HashSet<>()));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_given42_whenArrayListAdd42_thenArrayListSizeIsOne() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualRetainAllResult = objectList.retainAll(c);

    // Assert
    assertEquals(1, c.size());
    assertFalse(actualRetainAllResult);
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_given42_whenArrayListAdd42_thenArrayListSizeIsTwo() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act
    boolean actualRetainAllResult = objectList.retainAll(c);

    // Assert
    assertEquals(2, c.size());
    assertFalse(actualRetainAllResult);
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenIdentityStackAdd42_whenArrayList_thenReturnTrue() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    ArrayList<Object> c = new ArrayList<>();

    // Act
    boolean actualRetainAllResult = objectList.retainAll(c);

    // Assert
    assertTrue(objectList.isEmpty());
    assertTrue(actualRetainAllResult);
    assertEquals(objectList, c);
  }

  /**
   * Test {@link IdentityStack#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} is {@link IdentityStack#IdentityStack()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenIdentityStack_whenArrayList_thenArrayListIsIdentityStack() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    ArrayList<Object> c = new ArrayList<>();

    // Act and Assert
    assertFalse(objectList.retainAll(c));
    assertTrue(objectList.isEmpty());
    assertEquals(objectList, c);
  }

  /**
   * Test {@link IdentityStack#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When {@link HashSet#HashSet()}.</li>
   *   <li>Then {@link HashSet#HashSet()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenIdentityStack_whenHashSet_thenHashSetEmpty() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    HashSet<Object> c = new HashSet<>();

    // Act and Assert
    assertFalse(objectList.retainAll(c));
    assertTrue(c.isEmpty());
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link IdentityStack#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_given42_whenArrayListAdd42_thenReturnFalse() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(c));
  }

  /**
   * Test {@link IdentityStack#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_given42_whenArrayListAdd42_thenReturnFalse2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(c));
  }

  /**
   * Test {@link IdentityStack#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_givenIdentityStackAdd42_whenArrayList_thenReturnTrue() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }

  /**
   * Test {@link IdentityStack#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@link IdentityStack#IdentityStack()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_givenIdentityStack_whenArrayList_thenReturnTrue() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }
}
