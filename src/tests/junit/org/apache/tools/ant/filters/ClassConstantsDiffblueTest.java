package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ClassConstantsDiffblueTest {
  /**
   * Test {@link ClassConstants#ClassConstants(Reader)}.
   * <p>
   * Method under test: {@link ClassConstants#ClassConstants(Reader)}
   */
  @Test
  public void testNewClassConstants() {
    // Arrange and Act
    ClassConstants actualClassConstants = new ClassConstants(new StringReader("foo"));

    // Assert
    assertNull(actualClassConstants.getProject());
    assertFalse(actualClassConstants.getInitialized());
  }

  /**
   * Test {@link ClassConstants#ClassConstants()}.
   * <p>
   * Method under test: {@link ClassConstants#ClassConstants()}
   */
  @Test
  public void testNewClassConstants2() {
    // Arrange and Act
    ClassConstants actualClassConstants = new ClassConstants();

    // Assert
    assertNull(actualClassConstants.getProject());
    assertFalse(actualClassConstants.getInitialized());
  }

  /**
   * Test {@link ClassConstants#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassConstants#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new ClassConstants(new StringReader(""))).read());
  }

  /**
   * Test {@link ClassConstants#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassConstants#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ClassConstants(new StringReader("foo"))).read());
  }

  /**
   * Test {@link ClassConstants#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link ClassConstants}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassConstants#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnClassConstants() throws IOException {
    // Arrange
    ClassConstants classConstants = new ClassConstants();

    // Act
    Reader actualChainResult = classConstants.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof ClassConstants);
    assertEquals("foo", ((ClassConstants) actualChainResult).readFully());
    assertNull(((ClassConstants) actualChainResult).getProject());
    assertFalse(((ClassConstants) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }
}
