package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import org.junit.Test;

public class VectorSetDiffblueTest {
  /**
   * Method under test: {@link VectorSet#add(Object)}
   */
  @Test
  public void testAdd() {
    // Arrange, Act and Assert
    assertTrue((new VectorSet<>(1)).add("42"));
  }

  /**
   * Method under test: {@link VectorSet#add(Object)}
   */
  @Test
  public void testAdd2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertFalse(objectList.add("42"));
  }

  /**
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAll() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    // Act and Assert
    assertFalse(objectList.addAll(1, new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAll2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add(2);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.addAll(1, objectList1));
  }

  /**
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAll3() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add(42);
    objectList.add(2);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.addAll(1, objectList1));
  }

  /**
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAll4() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    // Act and Assert
    assertFalse(objectList.addAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAll5() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.addAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAll6() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.addAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#clone()}
   */
  @Test
  public void testClone() {
    // Arrange, Act and Assert
    assertTrue(((VectorSet<Object>) (new VectorSet<>(1)).clone()).isEmpty());
  }

  /**
  * Method under test: {@link VectorSet#VectorSet()}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new VectorSet<>()).isEmpty());
    assertTrue((new VectorSet<>(1)).isEmpty());
    assertTrue((new VectorSet<>(1, 3)).isEmpty());
    assertTrue((new VectorSet<>(new ArrayList<>())).isEmpty());
    assertTrue((new VectorSet<>((Collection<?>) null)).isEmpty());
  }

  /**
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testConstructor2() {
    // Arrange
    ArrayList<Object> objectList = new ArrayList<>();
    objectList.add("42");

    // Act and Assert
    assertEquals(1, (new VectorSet<>(objectList)).size());
  }

  /**
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testConstructor3() {
    // Arrange
    ArrayList<Object> objectList = new ArrayList<>();
    objectList.add("42");
    objectList.add("42");

    // Act and Assert
    assertEquals(1, (new VectorSet<>(objectList)).size());
  }

  /**
   * Method under test: {@link VectorSet#contains(Object)}
   */
  @Test
  public void testContains() {
    // Arrange, Act and Assert
    assertFalse((new VectorSet<>(1)).contains("42"));
  }

  /**
   * Method under test: {@link VectorSet#contains(Object)}
   */
  @Test
  public void testContains2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.contains("42"));
  }

  /**
   * Method under test: {@link VectorSet#containsAll(Collection)}
   */
  @Test
  public void testContainsAll() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#containsAll(Collection)}
   */
  @Test
  public void testContainsAll2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#remove(int)}
   */
  @Test
  public void testRemove() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertEquals("42", objectList.remove(0));
  }

  /**
   * Method under test: {@link VectorSet#remove(int)}
   */
  @Test
  public void testRemove2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(4);
    objectList.add("42");

    // Act and Assert
    assertEquals("42", objectList.remove(0));
  }

  /**
   * Method under test: {@link VectorSet#remove(Object)}
   */
  @Test
  public void testRemove3() {
    // Arrange, Act and Assert
    assertFalse((new VectorSet<>(1)).remove("42"));
  }

  /**
   * Method under test: {@link VectorSet#remove(Object)}
   */
  @Test
  public void testRemove4() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.remove("42"));
  }

  /**
   * Method under test: {@link VectorSet#remove(Object)}
   */
  @Test
  public void testRemove5() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(4);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.remove("42"));
  }

  /**
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    // Act and Assert
    assertFalse(objectList.removeAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertFalse(objectList.removeAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll3() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.removeAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll4() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(4);
    objectList.add("42");

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertTrue(objectList.removeAll(objectList1));
  }

  /**
   * Method under test: {@link VectorSet#removeElement(Object)}
   */
  @Test
  public void testRemoveElement() {
    // Arrange, Act and Assert
    assertFalse((new VectorSet<>(1)).removeElement("42"));
  }

  /**
   * Method under test: {@link VectorSet#removeElement(Object)}
   */
  @Test
  public void testRemoveElement2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.removeElement("42"));
  }

  /**
   * Method under test: {@link VectorSet#removeElement(Object)}
   */
  @Test
  public void testRemoveElement3() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(4);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.removeElement("42"));
  }

  /**
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);

    // Act and Assert
    assertFalse(objectList.retainAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.retainAll(new HashSet<>()));
  }

  /**
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll3() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.retainAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll4() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add(2);
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.retainAll(new ArrayList<>()));
  }

  /**
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll5() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>(1);
    objectList.add("42");

    ArrayList<Object> objectList1 = new ArrayList<>();
    objectList1.add("42");

    // Act and Assert
    assertFalse(objectList.retainAll(objectList1));
  }
}

