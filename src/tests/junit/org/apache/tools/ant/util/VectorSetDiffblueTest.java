package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import org.junit.Test;

public class VectorSetDiffblueTest {
  /**
   * Test {@link VectorSet#VectorSet()}.
   * <p>
   * Method under test: {@link VectorSet#VectorSet()}
   */
  @Test
  public void testNewVectorSet() {
    // Arrange and Act
    VectorSet<Object> actualObjectList = new VectorSet<>();

    // Assert
    assertTrue(actualObjectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#VectorSet(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@link ArrayList#ArrayList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testNewVectorSet_given42_whenArrayListAdd42_thenReturnArrayList() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertEquals(c, new VectorSet<>(c));
  }

  /**
   * Test {@link VectorSet#VectorSet(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testNewVectorSet_given42_whenArrayListAdd42_thenReturnSizeIsOne() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act
    VectorSet<Object> actualObjectList = new VectorSet<>(c);

    // Assert
    assertEquals(1, actualObjectList.size());
    assertEquals("42", actualObjectList.get(0));
  }

  /**
   * Test {@link VectorSet#VectorSet(Collection)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@link ArrayList#ArrayList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testNewVectorSet_whenArrayList_thenReturnArrayList() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();

    // Act and Assert
    assertEquals(c, new VectorSet<>(c));
  }

  /**
   * Test {@link VectorSet#VectorSet(Collection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(Collection)}
   */
  @Test
  public void testNewVectorSet_whenNull_thenReturnEmpty() {
    // Arrange and Act
    VectorSet<Object> actualObjectList = new VectorSet<>(null);

    // Assert
    assertTrue(actualObjectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#VectorSet(int)}.
   * <ul>
   *   <li>When one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(int)}
   */
  @Test
  public void testNewVectorSet_whenOne() {
    // Arrange and Act
    VectorSet<Object> actualObjectList = new VectorSet<>(1);

    // Assert
    assertTrue(actualObjectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#VectorSet(int, int)}.
   * <ul>
   *   <li>When three.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#VectorSet(int, int)}
   */
  @Test
  public void testNewVectorSet_whenThree() {
    // Arrange and Act
    VectorSet<Object> actualObjectList = new VectorSet<>(1, 3);

    // Assert
    assertTrue(actualObjectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#add(int, Object)} with {@code index}, {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#add(int, Object)}
   */
  @Test
  public void testAddWithIndexO_givenVectorSetAdd42_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    objectList.add(1, "42");

    // Assert that nothing has changed
    assertEquals(1, objectList.size());
  }

  /**
   * Test {@link VectorSet#add(int, Object)} with {@code index}, {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add forty-two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#add(int, Object)}
   */
  @Test
  public void testAddWithIndexO_givenVectorSetAddFortyTwo_thenVectorSetSizeIsThree() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(42);
    objectList.add(2);

    // Act
    objectList.add(1, "42");

    // Assert
    assertEquals(3, objectList.size());
    assertEquals("42", objectList.get(1));
    assertEquals(2, ((Integer) objectList.get(2)).intValue());
  }

  /**
   * Test {@link VectorSet#add(int, Object)} with {@code index}, {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#add(int, Object)}
   */
  @Test
  public void testAddWithIndexO_givenVectorSetAddTwo_thenVectorSetSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);

    // Act
    objectList.add(1, "42");

    // Assert
    assertEquals(2, objectList.size());
    assertEquals("42", objectList.get(1));
  }

  /**
   * Test {@link VectorSet#add(Object)} with {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#add(Object)}
   */
  @Test
  public void testAddWithO_givenVectorSetAdd42_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    boolean actualAddResult = objectList.add("42");

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertFalse(actualAddResult);
  }

  /**
   * Test {@link VectorSet#add(Object)} with {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#add(Object)}
   */
  @Test
  public void testAddWithO_givenVectorSet_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act
    boolean actualAddResult = objectList.add("42");

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertTrue(actualAddResult);
  }

  /**
   * Test {@link VectorSet#addElement(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addElement(Object)}
   */
  @Test
  public void testAddElement_givenVectorSet() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act
    objectList.addElement("42");

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
  }

  /**
   * Test {@link VectorSet#addElement(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addElement(Object)}
   */
  @Test
  public void testAddElement_givenVectorSetAdd42() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    objectList.addElement("42");

    // Assert that nothing has changed
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
  }

  /**
   * Test {@link VectorSet#addAll(Collection)} with {@code c}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAllWithC_given42_whenArrayListAdd42_thenArrayListSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act
    boolean actualAddAllResult = objectList.addAll(c);

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertEquals(2, c.size());
    assertTrue(actualAddAllResult);
  }

  /**
   * Test {@link VectorSet#addAll(Collection)} with {@code c}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAllWithC_given42_whenArrayListAdd42_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualAddAllResult = objectList.addAll(c);

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertTrue(actualAddAllResult);
    assertEquals(objectList, c);
  }

  /**
   * Test {@link VectorSet#addAll(Collection)} with {@code c}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(Collection)}
   */
  @Test
  public void testAddAllWithC_whenArrayList_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    ArrayList<Object> c = new ArrayList<>();

    // Act and Assert
    assertFalse(objectList.addAll(c));
    assertTrue(objectList.isEmpty());
    assertEquals(objectList, c);
  }

  /**
   * Test {@link VectorSet#addAll(int, Collection)} with {@code index}, {@code c}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add forty-two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAllWithIndexC_givenVectorSetAddFortyTwo_thenVectorSetSizeIsThree() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(42);
    objectList.add(2);

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualAddAllResult = objectList.addAll(1, c);

    // Assert
    assertEquals(3, objectList.size());
    assertEquals("42", objectList.get(1));
    assertEquals(2, ((Integer) objectList.get(2)).intValue());
    assertTrue(actualAddAllResult);
  }

  /**
   * Test {@link VectorSet#addAll(int, Collection)} with {@code index}, {@code c}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAllWithIndexC_givenVectorSetAddTwo_thenVectorSetSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualAddAllResult = objectList.addAll(1, c);

    // Assert
    assertEquals(2, objectList.size());
    assertEquals("42", objectList.get(1));
    assertTrue(actualAddAllResult);
  }

  /**
   * Test {@link VectorSet#addAll(int, Collection)} with {@code index}, {@code c}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#addAll(int, Collection)}
   */
  @Test
  public void testAddAllWithIndexC_givenVectorSet_whenArrayList_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.addAll(1, new ArrayList<>()));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#clone()}.
   * <p>
   * Method under test: {@link VectorSet#clone()}
   */
  @Test
  public void testClone() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertEquals(objectList, objectList.clone());
  }

  /**
   * Test {@link VectorSet#contains(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#contains(Object)}
   */
  @Test
  public void testContains_givenVectorSetAdd42_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act and Assert
    assertTrue(objectList.contains("42"));
  }

  /**
   * Test {@link VectorSet#contains(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#contains(Object)}
   */
  @Test
  public void testContains_givenVectorSet_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.contains("42"));
  }

  /**
   * Test {@link VectorSet#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_given42_whenArrayListAdd42_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(c));
  }

  /**
   * Test {@link VectorSet#containsAll(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_given42_whenArrayListAdd42_thenReturnFalse2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertFalse(objectList.containsAll(c));
  }

  /**
   * Test {@link VectorSet#containsAll(Collection)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#containsAll(Collection)}
   */
  @Test
  public void testContainsAll_whenArrayList_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertTrue(objectList.containsAll(new ArrayList<>()));
  }

  /**
   * Test {@link VectorSet#insertElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#insertElementAt(Object, int)}
   */
  @Test
  public void testInsertElementAt_givenVectorSetAdd42_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    objectList.insertElementAt("42", 1);

    // Assert that nothing has changed
    assertEquals(1, objectList.size());
  }

  /**
   * Test {@link VectorSet#insertElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add forty-two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#insertElementAt(Object, int)}
   */
  @Test
  public void testInsertElementAt_givenVectorSetAddFortyTwo_thenVectorSetSizeIsThree() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(42);
    objectList.add(2);

    // Act
    objectList.insertElementAt("42", 1);

    // Assert
    assertEquals(3, objectList.size());
    assertEquals("42", objectList.get(1));
    assertEquals(2, ((Integer) objectList.get(2)).intValue());
  }

  /**
   * Test {@link VectorSet#insertElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#insertElementAt(Object, int)}
   */
  @Test
  public void testInsertElementAt_givenVectorSetAddTwo_thenVectorSetSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);

    // Act
    objectList.insertElementAt("42", 1);

    // Assert
    assertEquals(2, objectList.size());
    assertEquals("42", objectList.get(1));
  }

  /**
   * Test {@link VectorSet#remove(int)} with {@code index}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When zero.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#remove(int)}
   */
  @Test
  public void testRemoveWithIndex_givenVectorSetAdd42_whenZero_thenReturn42() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act and Assert
    assertEquals("42", objectList.remove(0));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#remove(Object)} with {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#remove(Object)}
   */
  @Test
  public void testRemoveWithO_givenVectorSetAdd42_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    boolean actualRemoveResult = objectList.remove("42");

    // Assert
    assertTrue(objectList.isEmpty());
    assertTrue(actualRemoveResult);
  }

  /**
   * Test {@link VectorSet#remove(Object)} with {@code o}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#remove(Object)}
   */
  @Test
  public void testRemoveWithO_givenVectorSet_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.remove("42"));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenVectorSetAdd42_whenArrayListAdd42_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
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
   * Test {@link VectorSet#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenVectorSet_whenArrayListAdd42_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertFalse(objectList.removeAll(c));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenVectorSet_whenArrayListAdd42_thenReturnFalse2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertFalse(objectList.removeAll(c));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeAll(Collection)}
   */
  @Test
  public void testRemoveAll_givenVectorSet_whenArrayList_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.removeAll(new ArrayList<>()));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeElement(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeElement(Object)}
   */
  @Test
  public void testRemoveElement_givenVectorSetAdd42_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    boolean actualRemoveElementResult = objectList.removeElement("42");

    // Assert
    assertTrue(objectList.isEmpty());
    assertTrue(actualRemoveElementResult);
  }

  /**
   * Test {@link VectorSet#removeElement(Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeElement(Object)}
   */
  @Test
  public void testRemoveElement_givenVectorSet_thenReturnFalse() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act and Assert
    assertFalse(objectList.removeElement("42"));
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeElementAt(int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeElementAt(int)}
   */
  @Test
  public void testRemoveElementAt_givenVectorSetAdd42_whenZero_thenVectorSetEmpty() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    objectList.removeElementAt(0);

    // Assert
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeRange(int, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeRange(int, int)}
   */
  @Test
  public void testRemoveRange_givenVectorSetAdd42_whenZero_thenVectorSetEmpty() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    // Act
    objectList.removeRange(0, 1);

    // Assert
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#removeRange(int, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When one.</li>
   *   <li>Then {@link VectorSet#VectorSet()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#removeRange(int, int)}
   */
  @Test
  public void testRemoveRange_givenVectorSet_whenOne_thenVectorSetEmpty() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

    // Act
    objectList.removeRange(1, 1);

    // Assert that nothing has changed
    assertTrue(objectList.isEmpty());
  }

  /**
   * Test {@link VectorSet#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenVectorSetAdd42_whenArrayListAdd42_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add("42");

    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act
    boolean actualRetainAllResult = objectList.retainAll(c);

    // Assert
    assertEquals(1, objectList.size());
    assertFalse(actualRetainAllResult);
    assertEquals(objectList, c);
  }

  /**
   * Test {@link VectorSet#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenVectorSetAdd42_whenArrayList_thenReturnTrue() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
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
   * Test {@link VectorSet#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenVectorSet_whenArrayListAdd42_thenArrayListSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

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
   * Test {@link VectorSet#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenVectorSet_whenArrayListAdd42_thenArrayListSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();

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
   * Test {@link VectorSet#retainAll(Collection)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()}.</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} is {@link VectorSet#VectorSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#retainAll(Collection)}
   */
  @Test
  public void testRetainAll_givenVectorSet_whenArrayList_thenArrayListIsVectorSet() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    ArrayList<Object> c = new ArrayList<>();

    // Act and Assert
    assertFalse(objectList.retainAll(c));
    assertTrue(objectList.isEmpty());
    assertEquals(objectList, c);
  }

  /**
   * Test {@link VectorSet#set(int, Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add forty-two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#set(int, Object)}
   */
  @Test
  public void testSet_givenVectorSetAddFortyTwo_whenZero_thenVectorSetSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(42);
    objectList.add(2);
    objectList.add("42");

    // Act
    Object actualSetResult = objectList.set(0, "42");

    // Assert
    assertEquals(2, objectList.size());
    assertEquals("42", objectList.get(0));
    assertEquals(42, ((Integer) actualSetResult).intValue());
  }

  /**
   * Test {@link VectorSet#set(int, Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#set(int, Object)}
   */
  @Test
  public void testSet_givenVectorSetAddTwo_whenZero_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);
    objectList.add("42");

    // Act
    Object actualSetResult = objectList.set(0, "42");

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertEquals(2, ((Integer) actualSetResult).intValue());
  }

  /**
   * Test {@link VectorSet#set(int, Object)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#set(int, Object)}
   */
  @Test
  public void testSet_givenVectorSetAddTwo_whenZero_thenVectorSetSizeIsOne2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);

    // Act
    Object actualSetResult = objectList.set(0, "42");

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
    assertEquals(2, ((Integer) actualSetResult).intValue());
  }

  /**
   * Test {@link VectorSet#setElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add forty-two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#setElementAt(Object, int)}
   */
  @Test
  public void testSetElementAt_givenVectorSetAddFortyTwo_whenZero_thenVectorSetSizeIsTwo() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(42);
    objectList.add(2);
    objectList.add("42");

    // Act
    objectList.setElementAt("42", 0);

    // Assert
    assertEquals(2, objectList.size());
    assertEquals("42", objectList.get(0));
  }

  /**
   * Test {@link VectorSet#setElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#setElementAt(Object, int)}
   */
  @Test
  public void testSetElementAt_givenVectorSetAddTwo_whenZero_thenVectorSetSizeIsOne() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);
    objectList.add("42");

    // Act
    objectList.setElementAt("42", 0);

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
  }

  /**
   * Test {@link VectorSet#setElementAt(Object, int)}.
   * <ul>
   *   <li>Given {@link VectorSet#VectorSet()} add two.</li>
   *   <li>When zero.</li>
   *   <li>Then {@link VectorSet#VectorSet()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link VectorSet#setElementAt(Object, int)}
   */
  @Test
  public void testSetElementAt_givenVectorSetAddTwo_whenZero_thenVectorSetSizeIsOne2() {
    // Arrange
    VectorSet<Object> objectList = new VectorSet<>();
    objectList.add(2);

    // Act
    objectList.setElementAt("42", 0);

    // Assert
    assertEquals(1, objectList.size());
    assertEquals("42", objectList.get(0));
  }
}
