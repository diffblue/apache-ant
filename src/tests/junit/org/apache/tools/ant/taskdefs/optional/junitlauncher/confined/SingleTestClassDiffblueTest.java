package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class SingleTestClassDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link SingleTestClass}
   *   <li>{@link SingleTestClass#getName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    SingleTestClass actualSingleTestClass = new SingleTestClass();
    String actualName = actualSingleTestClass.getName();

    // Assert
    assertNull(actualSingleTestClass.getOutputDir());
    assertNull(actualSingleTestClass.getHaltOnFailure());
    assertNull(actualName);
    assertNull(actualSingleTestClass.getFailureProperty());
    assertNull(actualSingleTestClass.getIfProperty());
    assertNull(actualSingleTestClass.getUnlessProperty());
    assertNull(actualSingleTestClass.getForkDefinition());
    assertTrue(actualSingleTestClass.listeners.isEmpty());
  }

  /**
   * Test {@link SingleTestClass#setName(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setName(String)}
   */
  @Test
  public void testSetName_whenEmptyString_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new SingleTestClass()).setName(""));
  }

  /**
   * Test {@link SingleTestClass#setName(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setName(String)}
   */
  @Test
  public void testSetName_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new SingleTestClass()).setName(null));
  }

  /**
   * Test {@link SingleTestClass#setName(String)}.
   * <ul>
   *   <li>When {@code Test}.</li>
   *   <li>Then {@link SingleTestClass} (default constructor) Name is {@code Test}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setName(String)}
   */
  @Test
  public void testSetName_whenTest_thenSingleTestClassNameIsTest() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setName("Test");

    // Assert
    assertEquals("Test", singleTestClass.getName());
  }

  /**
   * Test {@link SingleTestClass#setMethods(String)}.
   * <ul>
   *   <li>When {@code ,}.</li>
   *   <li>Then not {@link SingleTestClass} (default constructor) hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setMethods(String)}
   */
  @Test
  public void testSetMethods_whenComma_thenNotSingleTestClassHasMethodsSpecified() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setMethods(",");

    // Assert that nothing has changed
    assertFalse(singleTestClass.hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#setMethods(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then not {@link SingleTestClass} (default constructor) hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setMethods(String)}
   */
  @Test
  public void testSetMethods_whenEmptyString_thenNotSingleTestClassHasMethodsSpecified() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setMethods("");

    // Assert that nothing has changed
    assertFalse(singleTestClass.hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#setMethods(String)}.
   * <ul>
   *   <li>When {@code Methods}.</li>
   *   <li>Then {@link SingleTestClass} (default constructor) hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setMethods(String)}
   */
  @Test
  public void testSetMethods_whenMethods_thenSingleTestClassHasMethodsSpecified() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setMethods("Methods");

    // Assert
    assertTrue(singleTestClass.hasMethodsSpecified());
    assertArrayEquals(new String[]{"Methods"}, singleTestClass.getMethods());
  }

  /**
   * Test {@link SingleTestClass#setMethods(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link SingleTestClass} (default constructor) hasMethodsSpecified.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#setMethods(String)}
   */
  @Test
  public void testSetMethods_whenNull_thenNotSingleTestClassHasMethodsSpecified() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();

    // Act
    singleTestClass.setMethods(null);

    // Assert that nothing has changed
    assertFalse(singleTestClass.hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#hasMethodsSpecified()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Methods is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#hasMethodsSpecified()}
   */
  @Test
  public void testHasMethodsSpecified_givenSingleTestClassMethodsIsFoo_thenReturnTrue() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setMethods("foo");

    // Act and Assert
    assertTrue(singleTestClass.hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#hasMethodsSpecified()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Methods is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#hasMethodsSpecified()}
   */
  @Test
  public void testHasMethodsSpecified_givenSingleTestClassMethodsIsNull_thenReturnFalse() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setMethods(null);

    // Act and Assert
    assertFalse(singleTestClass.hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#hasMethodsSpecified()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#hasMethodsSpecified()}
   */
  @Test
  public void testHasMethodsSpecified_givenSingleTestClass_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SingleTestClass()).hasMethodsSpecified());
  }

  /**
   * Test {@link SingleTestClass#getMethods()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Methods is {@code foo}.</li>
   *   <li>Then return array of {@link String} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#getMethods()}
   */
  @Test
  public void testGetMethods_givenSingleTestClassMethodsIsFoo_thenReturnArrayOfStringWithFoo() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setMethods("foo");

    // Act and Assert
    assertArrayEquals(new String[]{"foo"}, singleTestClass.getMethods());
  }

  /**
   * Test {@link SingleTestClass#getMethods()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor) Methods is {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#getMethods()}
   */
  @Test
  public void testGetMethods_givenSingleTestClassMethodsIsNull_thenReturnNull() {
    // Arrange
    SingleTestClass singleTestClass = new SingleTestClass();
    singleTestClass.setMethods(null);

    // Act and Assert
    assertNull(singleTestClass.getMethods());
  }

  /**
   * Test {@link SingleTestClass#getMethods()}.
   * <ul>
   *   <li>Given {@link SingleTestClass} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SingleTestClass#getMethods()}
   */
  @Test
  public void testGetMethods_givenSingleTestClass_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new SingleTestClass()).getMethods());
  }

  /**
   * Test {@link SingleTestClass#toForkedRepresentations()}.
   * <p>
   * Method under test: {@link SingleTestClass#toForkedRepresentations()}
   */
  @Test
  public void testToForkedRepresentations() throws IllegalStateException {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new SingleTestClass()).toForkedRepresentations());
  }
}
