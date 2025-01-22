package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.TimeComparison;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class AndDiffblueTest {
  /**
   * Test {@link And#And()}.
   * <p>
   * Method under test: {@link And#And()}
   */
  @Test
  public void testNewAnd() {
    // Arrange and Act
    And actualAnd = new And();

    // Assert
    Location location = actualAnd.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAnd.getDescription());
    assertNull(actualAnd.getProject());
    assertNull(actualAnd.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link And#And()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAndWithRIsAnd_whenResource_thenReturnTrue() {
    // Arrange
    And and = new And(new And());

    // Act and Assert
    assertTrue(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY} and {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAndWithRIsAnyAndAny_whenResource_thenReturnTrue() {
    // Arrange
    And and = new And(Type.ANY, Type.ANY);

    // Act and Assert
    assertTrue(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAndWithRIsAny_whenResource_thenReturnTrue() {
    // Arrange
    And and = new And(Type.ANY);

    // Act and Assert
    assertTrue(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link Majority#Majority()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAndWithRIsMajority_whenResource_thenReturnFalse() {
    // Arrange
    And and = new And(new Majority());

    // Act and Assert
    assertFalse(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And(ResourceSelector[])} with r is {@link None#None()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAndWithRIsNone_whenResource_thenReturnTrue() {
    // Arrange
    And and = new And(new None());

    // Act and Assert
    assertTrue(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link And#And()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAnd_whenResource_thenReturnTrue() {
    // Arrange
    And and = new And();

    // Act and Assert
    assertTrue(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Granularity is two.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateGranularityIsTwo_whenResource_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    And and = new And(date);

    // Act and Assert
    assertFalse(and.isSelected(new Resource()));
  }

  /**
   * Test {@link And#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    And and = new And(date);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(and.isSelected(r));
  }
}
