package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class LinkedHashtableDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LinkedHashtable#LinkedHashtable()}
   *   <li>{@link LinkedHashtable#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    LinkedHashtable<Object, Object> actualObjectObjectMap = new LinkedHashtable<>();

    // Assert
    assertEquals("{}", actualObjectObjectMap.toString());
  }

  /**
   * Test {@link LinkedHashtable#LinkedHashtable(Map)}.
   * <p>
   * Method under test: {@link LinkedHashtable#LinkedHashtable(Map)}
   */
  @Test
  public void testNewLinkedHashtable() {
    // Arrange
    HashMap<Object, Object> m = new HashMap<>();

    // Act and Assert
    assertEquals(m, new LinkedHashtable<>(m));
  }

  /**
   * Test {@link LinkedHashtable#LinkedHashtable(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#LinkedHashtable(int)}
   */
  @Test
  public void testNewLinkedHashtable_whenOne_thenReturnEmpty() {
    // Arrange and Act
    LinkedHashtable<Object, Object> actualObjectObjectMap = new LinkedHashtable<>(1);

    // Assert
    assertTrue(actualObjectObjectMap.isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#LinkedHashtable(int, float)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#LinkedHashtable(int, float)}
   */
  @Test
  public void testNewLinkedHashtable_whenOne_thenReturnEmpty2() {
    // Arrange and Act
    LinkedHashtable<Object, Object> actualObjectObjectMap = new LinkedHashtable<>(1, 10.0f);

    // Assert
    assertTrue(actualObjectObjectMap.isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#contains(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()} {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#contains(Object)}
   */
  @Test
  public void testContains_givenLinkedHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.contains("42"));
  }

  /**
   * Test {@link LinkedHashtable#contains(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()}.</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#contains(Object)}
   */
  @Test
  public void testContains_givenLinkedHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.contains("Value"));
  }

  /**
   * Test {@link LinkedHashtable#containsKey(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()} {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey_givenLinkedHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsKey("42"));
  }

  /**
   * Test {@link LinkedHashtable#containsKey(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()}.</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#containsKey(Object)}
   */
  @Test
  public void testContainsKey_givenLinkedHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.containsKey("Value"));
  }

  /**
   * Test {@link LinkedHashtable#containsValue(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()} {@code 42} is {@code 42}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue_givenLinkedHashtable42Is42_when42_thenReturnTrue() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertTrue(objectObjectMap.containsValue("42"));
  }

  /**
   * Test {@link LinkedHashtable#containsValue(Object)}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()}.</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#containsValue(Object)}
   */
  @Test
  public void testContainsValue_givenLinkedHashtable_whenValue_thenReturnFalse() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertFalse(objectObjectMap.containsValue("Value"));
  }

  /**
   * Test {@link LinkedHashtable#entrySet()}.
   * <p>
   * Method under test: {@link LinkedHashtable#entrySet()}
   */
  @Test
  public void testEntrySet() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertTrue(objectObjectMap.entrySet().isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}, and {@link LinkedHashtable#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    LinkedHashtable<Object, Object> objectObjectMap2 = new LinkedHashtable<>();

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap2);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap2.hashCode());
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}, and {@link LinkedHashtable#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    LinkedHashtable<Object, Object> objectObjectMap2 = new LinkedHashtable<>();
    objectObjectMap2.put("42", "42");

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap2);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap2.hashCode());
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}, and {@link LinkedHashtable#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LinkedHashtable#equals(Object)}
   *   <li>{@link LinkedHashtable#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertEquals(objectObjectMap, objectObjectMap);
    int expectedHashCodeResult = objectObjectMap.hashCode();
    assertEquals(expectedHashCodeResult, objectObjectMap.hashCode());
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertNotEquals(objectObjectMap, new LinkedHashtable<>());
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertNotEquals(objectObjectMap, null);
  }

  /**
   * Test {@link LinkedHashtable#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertNotEquals(objectObjectMap, "Different type to LinkedHashtable");
  }

  /**
   * Test {@link LinkedHashtable#get(Object)}.
   * <p>
   * Method under test: {@link LinkedHashtable#get(Object)}
   */
  @Test
  public void testGet() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertNull(objectObjectMap.get("42"));
  }

  /**
   * Test {@link LinkedHashtable#isEmpty()}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()} {@code 42} is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenLinkedHashtable42Is42_thenReturnFalse() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertFalse(objectObjectMap.isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#isEmpty()}.
   * <ul>
   *   <li>Given {@link LinkedHashtable#LinkedHashtable()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LinkedHashtable#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenLinkedHashtable_thenReturnTrue() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertTrue(objectObjectMap.isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#keySet()}.
   * <p>
   * Method under test: {@link LinkedHashtable#keySet()}
   */
  @Test
  public void testKeySet() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertTrue(objectObjectMap.keySet().isEmpty());
  }

  /**
   * Test {@link LinkedHashtable#put(Object, Object)}.
   * <p>
   * Method under test: {@link LinkedHashtable#put(Object, Object)}
   */
  @Test
  public void testPut() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act
    Object actualPutResult = objectObjectMap.put("42", "42");

    // Assert
    assertEquals(1, objectObjectMap.size());
    assertEquals("42", objectObjectMap.get("42"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LinkedHashtable#remove(Object)} with {@code Object}.
   * <p>
   * Method under test: {@link LinkedHashtable#remove(Object)}
   */
  @Test
  public void testRemoveWithObject() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertNull(objectObjectMap.remove("42"));
  }

  /**
   * Test {@link LinkedHashtable#size()}.
   * <p>
   * Method under test: {@link LinkedHashtable#size()}
   */
  @Test
  public void testSize() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertEquals(0, objectObjectMap.size());
  }

  /**
   * Test {@link LinkedHashtable#values()}.
   * <p>
   * Method under test: {@link LinkedHashtable#values()}
   */
  @Test
  public void testValues() {
    // Arrange
    LinkedHashtable<Object, Object> objectObjectMap = new LinkedHashtable<>();

    // Act and Assert
    assertTrue(objectObjectMap.values().isEmpty());
  }
}
