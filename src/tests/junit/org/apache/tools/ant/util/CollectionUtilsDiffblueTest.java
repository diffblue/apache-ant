package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.Vector;
import org.apache.tools.ant.util.CollectionUtils.EmptyEnumeration;
import org.junit.Test;

public class CollectionUtilsDiffblueTest {
  /**
   * Test EmptyEnumeration getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link EmptyEnumeration}
   *   <li>{@link EmptyEnumeration#hasMoreElements()}
   * </ul>
   */
  @Test
  public void testEmptyEnumerationGettersAndSetters() {
    // Arrange and Act
    EmptyEnumeration<Object> actualEmptyEnumeration = new EmptyEnumeration<>();

    // Assert
    assertFalse(actualEmptyEnumeration.hasMoreElements());
  }

  /**
   * Test EmptyEnumeration {@link EmptyEnumeration#nextElement()}.
   * <p>
   * Method under test: {@link EmptyEnumeration#nextElement()}
   */
  @Test
  public void testEmptyEnumerationNextElement() throws NoSuchElementException {
    // Arrange
    EmptyEnumeration<Object> emptyEnumeration = new EmptyEnumeration<>();

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> emptyEnumeration.nextElement());
  }

  /**
   * Test {@link CollectionUtils#equals(Dictionary, Dictionary)} with {@code d1}, {@code d2}.
   * <ul>
   *   <li>When {@link Hashtable#Hashtable(int)} with one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEqualsWithD1D2_whenHashtableWithOne_thenReturnTrue() {
    // Arrange
    Hashtable<?, ?> d1 = new Hashtable<>(1);

    // Act and Assert
    assertTrue(CollectionUtils.equals(d1, new Hashtable<>(1)));
  }

  /**
   * Test {@link CollectionUtils#equals(Dictionary, Dictionary)} with {@code d1}, {@code d2}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEqualsWithD1D2_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CollectionUtils.equals(null, new Hashtable<>(1)));
  }

  /**
   * Test {@link CollectionUtils#equals(Dictionary, Dictionary)} with {@code d1}, {@code d2}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEqualsWithD1D2_whenNull_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(CollectionUtils.equals(new Hashtable<>(1), null));
  }

  /**
   * Test {@link CollectionUtils#equals(Dictionary, Dictionary)} with {@code d1}, {@code d2}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEqualsWithD1D2_whenNull_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(CollectionUtils.equals((Dictionary<?, ?>) null, null));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_given42_whenStackAdd42_thenReturnFalse() {
    // Arrange
    Stack<Object> v1 = new Stack<>();
    v1.add("42");
    Vector<String> v2 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_given42_whenStackAdd42_thenReturnFalse2() {
    // Arrange
    Stack<Object> v1 = new Stack<>();
    v1.add("42");
    v1.add("42");
    Vector<String> v2 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_given42_whenStackAdd42_thenReturnFalse3() {
    // Arrange
    Vector<String> v1 = JavaEnvUtils.getJrePackageTestCases();

    Stack<Object> v2 = new Stack<>();
    v2.add("42");

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_given42_whenStackAdd42_thenReturnFalse4() {
    // Arrange
    Vector<String> v1 = JavaEnvUtils.getJrePackageTestCases();

    Stack<Object> v2 = new Stack<>();
    v2.add("42");
    v2.add("42");

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>When JrePackageTestCases.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_whenJrePackageTestCases_thenReturnTrue() {
    // Arrange
    Vector<String> v1 = JavaEnvUtils.getJrePackageTestCases();
    Vector<String> v2 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertTrue(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_whenStack_thenReturnFalse() {
    // Arrange
    Stack<Object> v1 = new Stack<>();
    Vector<String> v2 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, v2));
  }

  /**
   * Test {@link CollectionUtils#equals(Vector, Vector)} with {@code v1}, {@code v2}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEqualsWithV1V2_whenStack_thenReturnFalse2() {
    // Arrange
    Vector<String> v1 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, new Stack<>()));
  }

  /**
   * Test {@link CollectionUtils#flattenToString(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#flattenToString(Collection)}
   */
  @Test
  public void testFlattenToString_given42_whenArrayListAdd42_thenReturn42() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertEquals("42", CollectionUtils.flattenToString(c));
  }

  /**
   * Test {@link CollectionUtils#flattenToString(Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code 42,42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#flattenToString(Collection)}
   */
  @Test
  public void testFlattenToString_given42_whenArrayListAdd42_thenReturn4242() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertEquals("42,42", CollectionUtils.flattenToString(c));
  }

  /**
   * Test {@link CollectionUtils#flattenToString(Collection)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#flattenToString(Collection)}
   */
  @Test
  public void testFlattenToString_whenArrayList_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", CollectionUtils.flattenToString(new ArrayList<>()));
  }

  /**
   * Test {@link CollectionUtils#putAll(Dictionary, Dictionary)}.
   * <ul>
   *   <li>When {@link Hashtable#Hashtable(int)} with one.</li>
   *   <li>Then {@link Hashtable#Hashtable(int)} with one Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#putAll(Dictionary, Dictionary)}
   */
  @Test
  public void testPutAll_whenHashtableWithOne_thenHashtableWithOneEmpty() {
    // Arrange
    Hashtable<? super Object, ? super Object> m1 = new Hashtable<>(1);
    Hashtable<?, ?> m2 = new Hashtable<>(1);

    // Act
    CollectionUtils.putAll(m1, m2);

    // Assert that nothing has changed
    assertTrue(m1.isEmpty());
    assertTrue(m2.isEmpty());
  }

  /**
   * Test {@link CollectionUtils#asIterator(Enumeration)}.
   * <p>
   * Method under test: {@link CollectionUtils#asIterator(Enumeration)}
   */
  @Test
  public void testAsIterator() {
    // Arrange and Act
    Iterator<Object> actualAsIteratorResult = CollectionUtils.asIterator(new StringTokenizer("foo"));

    // Assert
    assertEquals("foo", actualAsIteratorResult.next());
    assertFalse(actualAsIteratorResult.hasNext());
  }

  /**
   * Test {@link CollectionUtils#asCollection(Iterator)}.
   * <p>
   * Method under test: {@link CollectionUtils#asCollection(Iterator)}
   */
  @Test
  public void testAsCollection() {
    // Arrange
    ArrayList<Object> objectList = new ArrayList<>();

    // Act
    Collection<Object> actualAsCollectionResult = CollectionUtils.asCollection(objectList.iterator());

    // Assert
    assertTrue(actualAsCollectionResult instanceof List);
    assertTrue(actualAsCollectionResult.isEmpty());
  }

  /**
   * Test {@link CollectionUtils#frequency(Collection, Object)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#frequency(Collection, Object)}
   */
  @Test
  public void testFrequency_given42_whenArrayListAdd42_thenReturnOne() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");

    // Act and Assert
    assertEquals(1, CollectionUtils.frequency(c, "42"));
  }

  /**
   * Test {@link CollectionUtils#frequency(Collection, Object)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#frequency(Collection, Object)}
   */
  @Test
  public void testFrequency_given42_whenArrayListAdd42_thenReturnTwo() {
    // Arrange
    ArrayList<Object> c = new ArrayList<>();
    c.add("42");
    c.add("42");

    // Act and Assert
    assertEquals(2, CollectionUtils.frequency(c, "42"));
  }

  /**
   * Test {@link CollectionUtils#frequency(Collection, Object)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#frequency(Collection, Object)}
   */
  @Test
  public void testFrequency_whenArrayList_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, CollectionUtils.frequency(new ArrayList<>(), "42"));
  }

  /**
   * Test {@link CollectionUtils#frequency(Collection, Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link CollectionUtils#frequency(Collection, Object)}
   */
  @Test
  public void testFrequency_whenNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, CollectionUtils.frequency(null, "42"));
  }
}
