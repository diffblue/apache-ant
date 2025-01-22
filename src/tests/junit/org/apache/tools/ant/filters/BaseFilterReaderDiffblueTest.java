package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.StringReader;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class BaseFilterReaderDiffblueTest {
  /**
   * Test {@link BaseFilterReader#read(char[], int, int)} with {@code char[]}, {@code int}, {@code int}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#read(char[], int, int)}
   */
  @Test
  public void testReadWithCharIntInt_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    ClassConstants classConstants = new ClassConstants(new StringReader(""));

    // Act and Assert
    assertEquals(-1, classConstants.read("AZAZ".toCharArray(), 1, 3));
  }

  /**
   * Test {@link BaseFilterReader#read(char[], int, int)} with {@code char[]}, {@code int}, {@code int}.
   * <ul>
   *   <li>When array of {@code char} with {@code A} and {@code Z}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#read(char[], int, int)}
   */
  @Test
  public void testReadWithCharIntInt_whenArrayOfCharWithAAndZ_thenReturnZero() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0, (new ClassConstants()).read(new char[]{'A', 'Z', 'A', 'Z'}, 1, 0));
  }

  /**
   * Test {@link BaseFilterReader#skip(long)}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#skip(long)}
   */
  @Test
  public void testSkip_givenClassConstants_whenMinusOne_thenThrowIllegalArgumentException()
      throws IOException, IllegalArgumentException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new ClassConstants()).skip(-1L));
  }

  /**
   * Test {@link BaseFilterReader#skip(long)}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   *   <li>When zero.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#skip(long)}
   */
  @Test
  public void testSkip_givenClassConstants_whenZero_thenReturnZero() throws IOException, IllegalArgumentException {
    // Arrange, Act and Assert
    assertEquals(0L, (new ClassConstants()).skip(0L));
  }

  /**
   * Test {@link BaseFilterReader#skip(long)}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>When one.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#skip(long)}
   */
  @Test
  public void testSkip_givenStringReaderWithEmptyString_whenOne_thenReturnZero()
      throws IOException, IllegalArgumentException {
    // Arrange, Act and Assert
    assertEquals(0L, (new ClassConstants(new StringReader(""))).skip(1L));
  }

  /**
   * Test {@link BaseFilterReader#setInitialized(boolean)}.
   * <p>
   * Method under test: {@link BaseFilterReader#setInitialized(boolean)}
   */
  @Test
  public void testSetInitialized() {
    // Arrange
    ClassConstants classConstants = new ClassConstants();

    // Act
    classConstants.setInitialized(true);

    // Assert
    assertTrue(classConstants.getInitialized());
  }

  /**
   * Test {@link BaseFilterReader#getInitialized()}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()} Initialized is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#getInitialized()}
   */
  @Test
  public void testGetInitialized_givenClassConstantsInitializedIsTrue_thenReturnTrue() {
    // Arrange
    ClassConstants classConstants = new ClassConstants();
    classConstants.setInitialized(true);

    // Act and Assert
    assertTrue(classConstants.getInitialized());
  }

  /**
   * Test {@link BaseFilterReader#getInitialized()}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#getInitialized()}
   */
  @Test
  public void testGetInitialized_givenClassConstants_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ClassConstants()).getInitialized());
  }

  /**
   * Test {@link BaseFilterReader#setProject(Project)}.
   * <p>
   * Method under test: {@link BaseFilterReader#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    ClassConstants classConstants = new ClassConstants();
    Project project = new Project();

    // Act
    classConstants.setProject(project);

    // Assert
    assertSame(project, classConstants.getProject());
  }

  /**
   * Test {@link BaseFilterReader#getProject()}.
   * <p>
   * Method under test: {@link BaseFilterReader#getProject()}
   */
  @Test
  public void testGetProject() {
    // Arrange, Act and Assert
    assertNull((new ClassConstants()).getProject());
  }

  /**
   * Test {@link BaseFilterReader#readLine()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#readLine()}
   */
  @Test
  public void testReadLine_givenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new ClassConstants(new StringReader(""))).readLine());
  }

  /**
   * Test {@link BaseFilterReader#readLine()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#readLine()}
   */
  @Test
  public void testReadLine_givenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", (new ClassConstants(new StringReader("foo"))).readLine());
  }

  /**
   * Test {@link BaseFilterReader#readFully()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#readFully()}
   */
  @Test
  public void testReadFully_givenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new ClassConstants(new StringReader(""))).readFully());
  }

  /**
   * Test {@link BaseFilterReader#readFully()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseFilterReader#readFully()}
   */
  @Test
  public void testReadFully_givenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", (new ClassConstants(new StringReader("foo"))).readFully());
  }
}
