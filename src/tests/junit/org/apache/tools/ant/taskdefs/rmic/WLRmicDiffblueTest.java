package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class WLRmicDiffblueTest {
  /**
   * Test {@link WLRmic#areIiopAndIdlSupported()}.
   * <p>
   * Method under test: {@link WLRmic#areIiopAndIdlSupported()}
   */
  @Test
  public void testAreIiopAndIdlSupported() {
    // Arrange, Act and Assert
    assertTrue((new WLRmic()).areIiopAndIdlSupported());
  }

  /**
   * Test {@link WLRmic#preprocessCompilerArgs(String[])}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Compiler Args}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLRmic#preprocessCompilerArgs(String[])}
   */
  @Test
  public void testPreprocessCompilerArgs_thenReturnArrayOfStringWithCompilerArgs() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Compiler Args"},
        (new WLRmic()).preprocessCompilerArgs(new String[]{"Compiler Args"}));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WLRmic}
   *   <li>{@link WLRmic#getSkelClassSuffix()}
   *   <li>{@link WLRmic#getStubClassSuffix()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    WLRmic actualWlRmic = new WLRmic();
    String actualSkelClassSuffix = actualWlRmic.getSkelClassSuffix();
    String actualStubClassSuffix = actualWlRmic.getStubClassSuffix();

    // Assert
    assertNull(actualWlRmic.getRmic());
    assertNull(actualWlRmic.getMapper());
    assertEquals(WLRmic.WL_RMI_SKEL_SUFFIX, actualSkelClassSuffix);
    assertEquals(WLRmic.WL_RMI_STUB_SUFFIX, actualStubClassSuffix);
  }
}
