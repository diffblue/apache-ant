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
  * Method under test: {@link IdentityStack#IdentityStack()}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new IdentityStack<>()).isEmpty());
    assertEquals(1, (new IdentityStack<>("42")).size());
  }

  /**
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains() {
    // Arrange, Act and Assert
    assertFalse((new IdentityStack<>()).contains("42"));
  }

  /**
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.contains("42"));
  }

  /**
   * Method under test: {@link IdentityStack#contains(Object)}
   */
  @Test
  public void testContains3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add(2);

    // Act and Assert
    assertFalse(objectList.contains("42"));
  }

  /**
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link IdentityStack#containsAll(Collection)}
   */
  @Test
  public void testContainsAll3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(objectList1));
  }

  /**
   * Method under test: {@link IdentityStack#getInstance(Stack)}
   */
  @Test
  public void testGetInstance() {
    // Arrange, Act and Assert
    assertTrue(IdentityStack.getInstance(new Stack<>()).isEmpty());
    assertTrue(IdentityStack.getInstance(null).isEmpty());
    assertTrue(IdentityStack.getInstance(new IdentityStack<>()).isEmpty());
  }

  /**
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOf() {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new IdentityStack<>()).indexOf("42", 1));
  }

  /**
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOf2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add("42");

    // Act and Assert
    assertEquals(1, objectList.indexOf("42", 1));
  }

  /**
   * Method under test: {@link IdentityStack#indexOf(Object, int)}
   */
  @Test
  public void testIndexOf3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add(2);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, objectList.indexOf("42", 1));
  }

  /**
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOf() {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new IdentityStack<>()).lastIndexOf("42", Retryable.RETRY_FOREVER));
  }

  /**
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOf2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add("42");

    // Act and Assert
    assertEquals(1, objectList.lastIndexOf("42", 1));
  }

  /**
   * Method under test: {@link IdentityStack#lastIndexOf(Object, int)}
   */
  @Test
  public void testLastIndexOf3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");
    objectList.add(2);

    // Act and Assert
    assertEquals(0, objectList.lastIndexOf("42", 1));
  }

  /**
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.removeAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.removeAll(new HashSet<>()));
  }

  /**
   * Method under test: {@link IdentityStack#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.removeAll(objectList1));
  }

  /**
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.retainAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll2() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();

    // Act and Assert
    assertFalse(objectList.retainAll(new HashSet<>()));
  }

  /**
   * Method under test: {@link IdentityStack#retainAll(Collection)}
   */
  @Test
  public void testRetainAll3() {
    // Arrange
    IdentityStack<Object> objectList = new IdentityStack<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.retainAll(new ArrayList<>()));
  }
}

