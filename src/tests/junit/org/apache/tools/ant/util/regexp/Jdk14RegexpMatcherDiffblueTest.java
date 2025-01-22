package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class Jdk14RegexpMatcherDiffblueTest {
  /**
   * Test {@link Jdk14RegexpMatcher#getCompiledPattern(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern_whenMatch_case_insensitive_thenReturnPattern() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertEquals("Pattern", jdk14RegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_CASE_INSENSITIVE).pattern());
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompiledPattern(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_MULTILINE}.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern_whenMatch_multiline_thenReturnPattern() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertEquals("Pattern", jdk14RegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_MULTILINE).pattern());
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompiledPattern(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_SINGLELINE}.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern_whenMatch_singleline_thenReturnPattern() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertEquals("Pattern", jdk14RegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_SINGLELINE).pattern());
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompiledPattern(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern_whenOne_thenReturnPattern() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertEquals("Pattern", jdk14RegexpMatcher.getCompiledPattern(1).pattern());
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String)} with {@code argument}.
   * <ul>
   *   <li>Given {@link Jdk14RegexpMatcher} (default constructor) Pattern is {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String)}
   */
  @Test
  public void testMatchesWithArgument_givenJdk14RegexpMatcherPatternIsPattern_thenReturnFalse() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Argument"));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String)} with {@code argument}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String)}
   */
  @Test
  public void testMatchesWithArgument_thenReturnTrue() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jdk14RegexpMatcher.matches("Argument"));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatchesWithInputOptions_thenReturnTrue() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jdk14RegexpMatcher.matches("Input", 1));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatchesWithInputOptions_whenMatch_case_insensitive_thenReturnFalse() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_MULTILINE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatchesWithInputOptions_whenMatch_multiline_thenReturnFalse() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_SINGLELINE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatchesWithInputOptions_whenMatch_singleline_thenReturnFalse() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#matches(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatchesWithInputOptions_whenOne_thenReturnFalse() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", 1));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String)} with {@code argument}.
   * <ul>
   *   <li>Given {@link Jdk14RegexpMatcher} (default constructor) Pattern is {@code Pattern}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroupsWithArgument_givenJdk14RegexpMatcherPatternIsPattern_thenReturnNull() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Argument"));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String)} with {@code argument}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroupsWithArgument_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jdk14RegexpMatcher.getGroups("Argument");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroupsWithInputOptions_thenReturnNull() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", 1));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroupsWithInputOptions_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jdk14RegexpMatcher.getGroups("Input", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroupsWithInputOptions_whenMatch_case_insensitive_thenReturnNull() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_MULTILINE}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroupsWithInputOptions_whenMatch_multiline_thenReturnNull() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getGroups(String, int)} with {@code input}, {@code options}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_SINGLELINE}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroupsWithInputOptions_whenMatch_singleline_thenReturnNull() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompilerOptions(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions_whenMatch_case_insensitive_thenReturnThree() {
    // Arrange, Act and Assert
    assertEquals(3, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompilerOptions(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_MULTILINE}.</li>
   *   <li>Then return nine.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions_whenMatch_multiline_thenReturnNine() {
    // Arrange, Act and Assert
    assertEquals(9, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompilerOptions(int)}.
   * <ul>
   *   <li>When {@link RegexpMatcher#MATCH_SINGLELINE}.</li>
   *   <li>Then return thirty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions_whenMatch_singleline_thenReturnThirtyThree() {
    // Arrange, Act and Assert
    assertEquals(33, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Test {@link Jdk14RegexpMatcher#getCompilerOptions(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions_whenOne_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new Jdk14RegexpMatcher()).getCompilerOptions(1));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Jdk14RegexpMatcher}
   *   <li>{@link Jdk14RegexpMatcher#setPattern(String)}
   *   <li>{@link Jdk14RegexpMatcher#getPattern()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Jdk14RegexpMatcher actualJdk14RegexpMatcher = new Jdk14RegexpMatcher();
    actualJdk14RegexpMatcher.setPattern("Pattern");

    // Assert
    assertEquals("Pattern", actualJdk14RegexpMatcher.getPattern());
  }
}
