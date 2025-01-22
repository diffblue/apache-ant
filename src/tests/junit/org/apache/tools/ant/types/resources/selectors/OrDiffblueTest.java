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

public class OrDiffblueTest {
  /**
   * Test {@link Or#Or()}.
   * <p>
   * Method under test: {@link Or#Or()}
   */
  @Test
  public void testNewOr() {
    // Arrange and Act
    Or actualOr = new Or();

    // Assert
    Location location = actualOr.getLocation();
    assertNull(location.getFileName());
    assertNull(actualOr.getDescription());
    assertNull(actualOr.getProject());
    assertNull(actualOr.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Granularity is two.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateGranularityIsTwo_whenResource_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    Or or = new Or(date);

    // Act and Assert
    assertFalse(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    Or or = new Or(date);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(or.isSelected(r));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or(ResourceSelector[])} with r is {@link And#And()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrWithRIsAnd_whenResource_thenReturnTrue() {
    // Arrange
    Or or = new Or(new And());

    // Act and Assert
    assertTrue(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrWithRIsAny_whenResource_thenReturnTrue() {
    // Arrange
    Or or = new Or(Type.ANY);

    // Act and Assert
    assertTrue(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or(ResourceSelector[])} with r is {@link Date} (default constructor) add {@link Type#ANY}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrWithRIsDateAddAny_whenResource_thenReturnTrue() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);

    Or or = new Or(date);
    or.add(Type.ANY);

    // Act and Assert
    assertTrue(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or(ResourceSelector[])} with r is {@link Majority#Majority()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrWithRIsMajority_whenResource_thenReturnFalse() {
    // Arrange
    Or or = new Or(new Majority());

    // Act and Assert
    assertFalse(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or(ResourceSelector[])} with r is {@link Or#Or()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrWithRIsOr_whenResource_thenReturnFalse() {
    // Arrange
    Or or = new Or(new Or());

    // Act and Assert
    assertFalse(or.isSelected(new Resource()));
  }

  /**
   * Test {@link Or#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Or#Or()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOr_whenResource_thenReturnFalse() {
    // Arrange
    Or or = new Or();

    // Act and Assert
    assertFalse(or.isSelected(new Resource()));
  }
}
