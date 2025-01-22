package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.taskdefs.Rmic;
import org.junit.Test;

public class DefaultRmicAdapterDiffblueTest {
  /**
   * Test {@link DefaultRmicAdapter#setRmic(Rmic)}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#setRmic(Rmic)}
   */
  @Test
  public void testSetRmic() {
    // Arrange
    ForkingSunRmic forkingSunRmic = new ForkingSunRmic();
    Rmic attributes = new Rmic();

    // Act
    forkingSunRmic.setRmic(attributes);

    // Assert
    assertSame(attributes, forkingSunRmic.getRmic());
  }

  /**
   * Test {@link DefaultRmicAdapter#getRmic()}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#getRmic()}
   */
  @Test
  public void testGetRmic() {
    // Arrange, Act and Assert
    assertNull((new ForkingSunRmic()).getRmic());
  }

  /**
   * Test {@link DefaultRmicAdapter#getStubClassSuffix()}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#getStubClassSuffix()}
   */
  @Test
  public void testGetStubClassSuffix() {
    // Arrange, Act and Assert
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, (new ForkingSunRmic()).getStubClassSuffix());
  }

  /**
   * Test {@link DefaultRmicAdapter#getSkelClassSuffix()}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#getSkelClassSuffix()}
   */
  @Test
  public void testGetSkelClassSuffix() {
    // Arrange, Act and Assert
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, (new ForkingSunRmic()).getSkelClassSuffix());
  }

  /**
   * Test {@link DefaultRmicAdapter#getTieClassSuffix()}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#getTieClassSuffix()}
   */
  @Test
  public void testGetTieClassSuffix() {
    // Arrange, Act and Assert
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, (new ForkingSunRmic()).getTieClassSuffix());
  }

  /**
   * Test {@link DefaultRmicAdapter#getMapper()}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#getMapper()}
   */
  @Test
  public void testGetMapper() {
    // Arrange, Act and Assert
    assertNull((new ForkingSunRmic()).getMapper());
  }

  /**
   * Test {@link DefaultRmicAdapter#areIiopAndIdlSupported()}.
   * <ul>
   *   <li>Given {@link SunRmic} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultRmicAdapter#areIiopAndIdlSupported()}
   */
  @Test
  public void testAreIiopAndIdlSupported_givenSunRmic_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SunRmic()).areIiopAndIdlSupported());
  }

  /**
   * Test {@link DefaultRmicAdapter#preprocessCompilerArgs(String[])}.
   * <p>
   * Method under test: {@link DefaultRmicAdapter#preprocessCompilerArgs(String[])}
   */
  @Test
  public void testPreprocessCompilerArgs() {
    // Arrange
    String[] compilerArgs = new String[]{"Compiler Args"};

    // Act and Assert
    assertSame(compilerArgs, (new ForkingSunRmic()).preprocessCompilerArgs(compilerArgs));
  }

  /**
   * Test {@link DefaultRmicAdapter#filterJvmCompilerArgs(String[])}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Compiler Args}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultRmicAdapter#filterJvmCompilerArgs(String[])}
   */
  @Test
  public void testFilterJvmCompilerArgs_thenReturnArrayOfStringWithCompilerArgs() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Compiler Args"},
        (new ForkingSunRmic()).filterJvmCompilerArgs(new String[]{"Compiler Args"}));
  }
}
