package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.TimeComparison;
import org.junit.Test;

public class NotDiffblueTest {
  /**
   * Test {@link Not#Not(ResourceSelector)}.
   * <p>
   * Method under test: {@link Not#Not(ResourceSelector)}
   */
  @Test
  public void testNewNot() {
    // Arrange, Act and Assert
    assertFalse((new Not(Type.ANY)).isSelected(null));
  }

  /**
   * Test {@link Not#add(ResourceSelector)}.
   * <ul>
   *   <li>Given {@link Not#Not(ResourceSelector)} with s is {@link Type#ANY}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#add(ResourceSelector)}
   */
  @Test
  public void testAdd_givenNotWithSIsAny_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new Not(Type.ANY)).add(Type.ANY));
  }

  /**
   * Test {@link Not#add(ResourceSelector)}.
   * <ul>
   *   <li>Given {@link Not#Not()}.</li>
   *   <li>Then not {@link Not#Not()} Selected is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#add(ResourceSelector)}
   */
  @Test
  public void testAdd_givenNot_thenNotNotSelectedIsNull() {
    // Arrange
    Not not = new Not();

    // Act
    not.add(Type.ANY);

    // Assert
    assertFalse(not.isSelected(null));
  }

  /**
   * Test {@link Not#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Granularity is two.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateGranularityIsTwo_thenReturnTrue() {
    // Arrange
    Date s = new Date();
    s.setGranularity(2L);
    s.setMillis(2L);
    s.setPattern("foo");
    s.setWhen(TimeComparison.AFTER);
    Not not = new Not(s);

    // Act and Assert
    assertTrue(not.isSelected(new Resource()));
  }

  /**
   * Test {@link Not#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Not#Not()} add {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNotAddAny_thenReturnTrue() {
    // Arrange
    Not s = new Not();
    s.add(Type.ANY);
    Not not = new Not(s);

    // Act and Assert
    assertTrue(not.isSelected(new Resource()));
  }

  /**
   * Test {@link Not#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Not#Not(ResourceSelector)} with s is {@link Type#ANY}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNotWithSIsAny_thenReturnFalse() {
    // Arrange
    Not not = new Not(Type.ANY);

    // Act and Assert
    assertFalse(not.isSelected(new Resource()));
  }
}
