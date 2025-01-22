package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.List;
import junit.framework.JUnit4TestCaseFacade;
import org.junit.Test;
import org.junit.runner.Description;

public class JUnit4TestMethodAdapterDiffblueTest {
  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenArrayOfStringWithEmptyString() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new JUnit4TestMethodAdapter(testClass, new String[]{""}));

  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Method Names}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenArrayOfStringWithMethodNames() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act
    JUnit4TestMethodAdapter actualJUnit4TestMethodAdapter = new JUnit4TestMethodAdapter(testClass,
        new String[]{"Method Names"});

    // Assert
    Description description = actualJUnit4TestMethodAdapter.getDescription();
    Collection<Annotation> annotations = description.getAnnotations();
    assertTrue(annotations instanceof List);
    List<junit.framework.Test> tests = actualJUnit4TestMethodAdapter.getTests();
    assertEquals(1, tests.size());
    assertTrue(tests.get(0) instanceof JUnit4TestCaseFacade);
    assertEquals("java.lang.Object", description.getClassName());
    assertEquals("java.lang.Object", description.getDisplayName());
    assertNull(description.getMethodName());
    assertEquals(1, description.getChildren().size());
    assertFalse(description.isEmpty());
    assertFalse(description.isTest());
    assertTrue(annotations.isEmpty());
    assertTrue(description.isSuite());
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, actualJUnit4TestMethodAdapter.getTestClass());
    assertSame(testClass, description.getTestClass());
  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Method Names} and {@code Method Names}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenArrayOfStringWithMethodNamesAndMethodNames() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act
    JUnit4TestMethodAdapter actualJUnit4TestMethodAdapter = new JUnit4TestMethodAdapter(testClass,
        new String[]{"Method Names", "Method Names"});

    // Assert
    Description description = actualJUnit4TestMethodAdapter.getDescription();
    Collection<Annotation> annotations = description.getAnnotations();
    assertTrue(annotations instanceof List);
    List<junit.framework.Test> tests = actualJUnit4TestMethodAdapter.getTests();
    assertEquals(1, tests.size());
    assertTrue(tests.get(0) instanceof JUnit4TestCaseFacade);
    assertEquals("java.lang.Object", description.getClassName());
    assertEquals("java.lang.Object", description.getDisplayName());
    assertNull(description.getMethodName());
    assertEquals(1, description.getChildren().size());
    assertFalse(description.isEmpty());
    assertFalse(description.isTest());
    assertTrue(annotations.isEmpty());
    assertTrue(description.isSuite());
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, actualJUnit4TestMethodAdapter.getTestClass());
    assertSame(testClass, description.getTestClass());
  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenArrayOfStringWithNull() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new JUnit4TestMethodAdapter(testClass, new String[]{null}));

  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenEmptyArrayOfString() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act
    JUnit4TestMethodAdapter actualJUnit4TestMethodAdapter = new JUnit4TestMethodAdapter(testClass, new String[]{});

    // Assert
    Description description = actualJUnit4TestMethodAdapter.getDescription();
    Collection<Annotation> annotations = description.getAnnotations();
    assertTrue(annotations instanceof List);
    List<junit.framework.Test> tests = actualJUnit4TestMethodAdapter.getTests();
    assertEquals(1, tests.size());
    assertTrue(tests.get(0) instanceof JUnit4TestCaseFacade);
    assertEquals("java.lang.Object", description.getClassName());
    assertEquals("java.lang.Object", description.getDisplayName());
    assertNull(description.getMethodName());
    assertEquals(1, description.getChildren().size());
    assertFalse(description.isEmpty());
    assertFalse(description.isTest());
    assertTrue(annotations.isEmpty());
    assertTrue(description.isSuite());
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, actualJUnit4TestMethodAdapter.getTestClass());
    assertSame(testClass, description.getTestClass());
  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new JUnit4TestMethodAdapter(null, new String[]{"Method Names"}));

  }

  /**
   * Test {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#JUnit4TestMethodAdapter(Class, String[])}
   */
  @Test
  public void testNewJUnit4TestMethodAdapter_whenNull_thenThrowIllegalArgumentException2() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new JUnit4TestMethodAdapter(testClass, null));

  }

  /**
   * Test {@link JUnit4TestMethodAdapter#countTestCases()}.
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#countTestCases()}
   */
  @Test
  public void testCountTestCases() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act and Assert
    assertEquals(1, (new JUnit4TestMethodAdapter(testClass, new String[]{"Method Names"})).countTestCases());
  }

  /**
   * Test {@link JUnit4TestMethodAdapter#getDescription()}.
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#getDescription()}
   */
  @Test
  public void testGetDescription() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act
    Description actualDescription = (new JUnit4TestMethodAdapter(testClass, new String[]{"Method Names"}))
        .getDescription();

    // Assert
    Collection<Annotation> annotations = actualDescription.getAnnotations();
    assertTrue(annotations instanceof List);
    assertEquals("java.lang.Object", actualDescription.getClassName());
    assertEquals("java.lang.Object", actualDescription.getDisplayName());
    assertNull(actualDescription.getMethodName());
    assertEquals(1, actualDescription.getChildren().size());
    assertFalse(actualDescription.isEmpty());
    assertFalse(actualDescription.isTest());
    assertTrue(annotations.isEmpty());
    assertTrue(actualDescription.isSuite());
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, actualDescription.getTestClass());
  }

  /**
   * Test {@link JUnit4TestMethodAdapter#getTests()}.
   * <p>
   * Method under test: {@link JUnit4TestMethodAdapter#getTests()}
   */
  @Test
  public void testGetTests() {
    // Arrange
    Class<Object> testClass = Object.class;

    // Act
    List<junit.framework.Test> actualTests = (new JUnit4TestMethodAdapter(testClass, new String[]{"Method Names"}))
        .getTests();

    // Assert
    assertEquals(1, actualTests.size());
    junit.framework.Test getResult = actualTests.get(0);
    Description description = ((JUnit4TestCaseFacade) getResult).getDescription();
    Collection<Annotation> annotations = description.getAnnotations();
    assertTrue(annotations instanceof List);
    assertTrue(getResult instanceof JUnit4TestCaseFacade);
    assertEquals("initializationError", description.getMethodName());
    assertEquals("initializationError(java.lang.Object)", description.getDisplayName());
    assertEquals("java.lang.Object", description.getClassName());
    assertFalse(description.isEmpty());
    assertFalse(description.isSuite());
    assertTrue(description.getChildren().isEmpty());
    assertTrue(annotations.isEmpty());
    assertTrue(description.isTest());
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, description.getTestClass());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JUnit4TestMethodAdapter#toString()}
   *   <li>{@link JUnit4TestMethodAdapter#getTestClass()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Class<Object> testClass = Object.class;
    JUnit4TestMethodAdapter jUnit4TestMethodAdapter = new JUnit4TestMethodAdapter(testClass,
        new String[]{"Method Names"});

    // Act
    String actualToStringResult = jUnit4TestMethodAdapter.toString();
    Class<?> actualTestClass = jUnit4TestMethodAdapter.getTestClass();

    // Assert
    assertEquals(":Method Names", actualToStringResult);
    Class<Object> expectedTestClass = Object.class;
    assertEquals(expectedTestClass, actualTestClass);
    assertSame(testClass, actualTestClass);
  }
}
