package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class DelegatedResourceComparatorDiffblueTest {
  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}, and {@link DelegatedResourceComparator#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DelegatedResourceComparator#equals(Object)}
   *   <li>{@link DelegatedResourceComparator#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    DelegatedResourceComparator delegatedResourceComparator = new DelegatedResourceComparator();
    DelegatedResourceComparator delegatedResourceComparator2 = new DelegatedResourceComparator();

    // Act and Assert
    assertEquals(delegatedResourceComparator, delegatedResourceComparator2);
    int expectedHashCodeResult = delegatedResourceComparator.hashCode();
    assertEquals(expectedHashCodeResult, delegatedResourceComparator2.hashCode());
  }

  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}, and {@link DelegatedResourceComparator#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DelegatedResourceComparator#equals(Object)}
   *   <li>{@link DelegatedResourceComparator#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    DelegatedResourceComparator delegatedResourceComparator = new DelegatedResourceComparator();

    // Act and Assert
    assertEquals(delegatedResourceComparator, delegatedResourceComparator);
    int expectedHashCodeResult = delegatedResourceComparator.hashCode();
    assertEquals(expectedHashCodeResult, delegatedResourceComparator.hashCode());
  }

  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DelegatedResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    DelegatedResourceComparator delegatedResourceComparator = new DelegatedResourceComparator();
    delegatedResourceComparator.add(new Content());

    // Act and Assert
    assertNotEquals(delegatedResourceComparator, new DelegatedResourceComparator());
  }

  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DelegatedResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    DelegatedResourceComparator delegatedResourceComparator = new DelegatedResourceComparator();

    DelegatedResourceComparator delegatedResourceComparator2 = new DelegatedResourceComparator();
    delegatedResourceComparator2.add(new Content());

    // Act and Assert
    assertNotEquals(delegatedResourceComparator, delegatedResourceComparator2);
  }

  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DelegatedResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new DelegatedResourceComparator(), null);
  }

  /**
   * Test {@link DelegatedResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link DelegatedResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new DelegatedResourceComparator(), "Different type to DelegatedResourceComparator");
  }

  /**
   * Test {@link DelegatedResourceComparator#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@link DelegatedResourceComparator} (default constructor) add {@link Date} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DelegatedResourceComparator#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenDelegatedResourceComparatorAddDate_thenReturnZero() {
    // Arrange
    DelegatedResourceComparator delegatedResourceComparator = new DelegatedResourceComparator();
    delegatedResourceComparator.add(new Date());
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, delegatedResourceComparator.resourceCompare(foo, new Resource()));
  }

  /**
   * Test new {@link DelegatedResourceComparator} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DelegatedResourceComparator}
   */
  @Test
  public void testNewDelegatedResourceComparator() {
    // Arrange and Act
    DelegatedResourceComparator actualDelegatedResourceComparator = new DelegatedResourceComparator();

    // Assert
    Location location = actualDelegatedResourceComparator.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDelegatedResourceComparator.getDescription());
    assertNull(actualDelegatedResourceComparator.getProject());
    assertNull(actualDelegatedResourceComparator.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
