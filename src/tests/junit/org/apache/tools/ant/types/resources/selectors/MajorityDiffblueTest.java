package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.TimeComparison;
import org.junit.Test;

public class MajorityDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Majority#Majority()}
   *   <li>{@link Majority#setAllowtie(boolean)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Majority actualMajority = new Majority();
    actualMajority.setAllowtie(true);

    // Assert
    Location location = actualMajority.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMajority.getDescription());
    assertNull(actualMajority.getProject());
    assertNull(actualMajority.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link And#And()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsAnd_thenReturnTrue() {
    // Arrange
    Majority majority = new Majority(new And());

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Type#ANY} and {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsAnyAndAny_thenReturnTrue() {
    // Arrange
    Majority majority = new Majority(Type.ANY, Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Type#ANY} and {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsAnyAndAny_thenReturnTrue2() {
    // Arrange
    Majority majority = new Majority(Type.ANY, Type.ANY, Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsAny_thenReturnTrue() {
    // Arrange
    Majority majority = new Majority(Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Date} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsDateAddAny_thenReturnTrue() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);

    Majority majority = new Majority(date);
    majority.add(Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Date} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsDateAddAny_thenReturnTrue2() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);

    Majority majority = new Majority(date);
    majority.add(Type.ANY);
    majority.add(Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Date} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsDateAddAny_thenReturnTrue3() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);

    Majority majority = new Majority(date);
    majority.add(Type.ANY);
    majority.add(Type.ANY);
    majority.add(Type.ANY);

    // Act and Assert
    assertTrue(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Date} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsDate_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setGranularity(2L);
    date.setMillis(2L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    Majority majority = new Majority(date);

    // Act and Assert
    assertFalse(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority(ResourceSelector[])} with r is {@link Majority#Majority()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajorityWithRIsMajority_thenReturnFalse() {
    // Arrange
    Majority majority = new Majority(new Majority());

    // Act and Assert
    assertFalse(majority.isSelected(new Resource()));
  }

  /**
   * Test {@link Majority#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Majority#Majority()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Majority#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenMajority_thenReturnFalse() {
    // Arrange
    Majority majority = new Majority();

    // Act and Assert
    assertFalse(majority.isSelected(new Resource()));
  }
}
