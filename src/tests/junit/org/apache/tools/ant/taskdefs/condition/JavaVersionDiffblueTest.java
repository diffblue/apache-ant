package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JavaVersionDiffblueTest {
  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtLeast is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtLeastIs42_thenReturnFalse() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast("42");
    javaVersion.setExactly(null);
    javaVersion.setAtMost("foo");

    // Act and Assert
    assertFalse(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtLeast is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtLeastIsEmptyString_thenReturnTrue() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast("");
    javaVersion.setExactly(null);
    javaVersion.setAtMost("foo");

    // Act and Assert
    assertTrue(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtLeastIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast("foo");
    javaVersion.setExactly(null);
    javaVersion.setAtMost(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtLeastIsFoo_thenThrowBuildException2() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast("foo");
    javaVersion.setExactly("foo");
    javaVersion.setAtMost(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtLeastIsFoo_thenThrowBuildException3() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast("foo");
    javaVersion.setExactly("foo");
    javaVersion.setAtMost("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtMost is {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtMostIs42_thenReturnTrue() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly(null);
    javaVersion.setAtMost("42");

    // Act and Assert
    assertTrue(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtMost is empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtMostIsEmptyString_thenReturnFalse() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly(null);
    javaVersion.setAtMost("");

    // Act and Assert
    assertFalse(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) AtMost is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionAtMostIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly(null);
    javaVersion.setAtMost("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) Exactly is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionExactlyIs42_thenReturnFalse() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly("42");
    javaVersion.setAtMost("42");

    // Act and Assert
    assertFalse(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) Exactly is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionExactlyIs42_thenReturnFalse2() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly("42");
    javaVersion.setAtMost(null);

    // Act and Assert
    assertFalse(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) Exactly is empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionExactlyIsEmptyString_thenReturnFalse() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly("");
    javaVersion.setAtMost("42");

    // Act and Assert
    assertFalse(javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor) Exactly is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersionExactlyIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    JavaVersion javaVersion = new JavaVersion();
    javaVersion.setAtLeast(null);
    javaVersion.setExactly("foo");
    javaVersion.setAtMost(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> javaVersion.eval());
  }

  /**
   * Test {@link JavaVersion#eval()}.
   * <ul>
   *   <li>Given {@link JavaVersion} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaVersion#eval()}
   */
  @Test
  public void testEval_givenJavaVersion_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JavaVersion()).eval());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JavaVersion}
   *   <li>{@link JavaVersion#setAtLeast(String)}
   *   <li>{@link JavaVersion#setAtMost(String)}
   *   <li>{@link JavaVersion#setExactly(String)}
   *   <li>{@link JavaVersion#getAtLeast()}
   *   <li>{@link JavaVersion#getAtMost()}
   *   <li>{@link JavaVersion#getExactly()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JavaVersion actualJavaVersion = new JavaVersion();
    actualJavaVersion.setAtLeast("At Least");
    actualJavaVersion.setAtMost("At Most");
    actualJavaVersion.setExactly("Exactly");
    String actualAtLeast = actualJavaVersion.getAtLeast();
    String actualAtMost = actualJavaVersion.getAtMost();

    // Assert
    assertEquals("At Least", actualAtLeast);
    assertEquals("At Most", actualAtMost);
    assertEquals("Exactly", actualJavaVersion.getExactly());
  }
}
