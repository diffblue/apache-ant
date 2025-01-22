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

public class NoneDiffblueTest {
  /**
   * Test {@link None#None()}.
   * <p>
   * Method under test: {@link None#None()}
   */
  @Test
  public void testNewNone() {
    // Arrange and Act
    None actualNone = new None();

    // Assert
    Location location = actualNone.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNone.getDescription());
    assertNull(actualNone.getProject());
    assertNull(actualNone.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Granularity is two.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateGranularityIsTwo_whenResource_thenReturnTrue() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    None none = new None(date);

    // Act and Assert
    assertTrue(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    None none = new None(date);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertTrue(none.isSelected(r));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None(ResourceSelector[])} with r is {@link And#And()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNoneWithRIsAnd_whenResource_thenReturnFalse() {
    // Arrange
    None none = new None(new And());

    // Act and Assert
    assertFalse(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNoneWithRIsAny_whenResource_thenReturnFalse() {
    // Arrange
    None none = new None(Type.ANY);

    // Act and Assert
    assertFalse(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None(ResourceSelector[])} with r is {@link Date} (default constructor) add {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNoneWithRIsDateAddAny_whenResource_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);

    None none = new None(date);
    none.add(Type.ANY);

    // Act and Assert
    assertFalse(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None(ResourceSelector[])} with r is {@link Majority#Majority()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNoneWithRIsMajority_whenResource_thenReturnTrue() {
    // Arrange
    None none = new None(new Majority());

    // Act and Assert
    assertTrue(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None(ResourceSelector[])} with r is {@link None#None()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNoneWithRIsNone_whenResource_thenReturnFalse() {
    // Arrange
    None none = new None(new None());

    // Act and Assert
    assertFalse(none.isSelected(new Resource()));
  }

  /**
   * Test {@link None#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link None#None()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link None#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNone_whenResource_thenReturnTrue() {
    // Arrange
    None none = new None();

    // Act and Assert
    assertTrue(none.isSelected(new Resource()));
  }
}
