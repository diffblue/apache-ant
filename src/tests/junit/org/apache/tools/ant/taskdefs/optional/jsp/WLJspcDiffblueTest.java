package org.apache.tools.ant.taskdefs.optional.jsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class WLJspcDiffblueTest {
  /**
   * Test {@link WLJspc#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    WLJspc wlJspc = new WLJspc();
    wlJspc.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> wlJspc.execute());
  }

  /**
   * Test {@link WLJspc#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link WLJspc} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenWLJspcRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    WLJspc wlJspc = new WLJspc();
    wlJspc.setClasspath(Path.systemBootClasspath);

    // Act
    wlJspc.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(wlJspc.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link WLJspc#createClasspath()}.
   * <ul>
   *   <li>Given {@link WLJspc} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenWLJspc_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new WLJspc()).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link WLJspc#createClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnSystemBootClasspath() {
    // Arrange
    WLJspc wlJspc = new WLJspc();
    wlJspc.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualCreateClasspathResult = wlJspc.createClasspath();

    // Assert
    assertSame(actualCreateClasspathResult.systemBootClasspath, actualCreateClasspathResult);
  }

  /**
   * Test {@link WLJspc#replaceString(String, String, String)}.
   * <ul>
   *   <li>When {@code Escape Chars}.</li>
   *   <li>Then return {@code Inp String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#replaceString(String, String, String)}
   */
  @Test
  public void testReplaceString_whenEscapeChars_thenReturnInpString() {
    // Arrange, Act and Assert
    assertEquals("Inp String", (new WLJspc()).replaceString("Inp String", "Escape Chars", "Replace Chars"));
  }

  /**
   * Test {@link WLJspc#replaceString(String, String, String)}.
   * <ul>
   *   <li>When {@code p}.</li>
   *   <li>Then return {@code InReplace Chars String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WLJspc#replaceString(String, String, String)}
   */
  @Test
  public void testReplaceString_whenP_thenReturnInReplaceCharsString() {
    // Arrange, Act and Assert
    assertEquals("InReplace Chars String", (new WLJspc()).replaceString("Inp String", "p", "Replace Chars"));
  }

  /**
   * Test new {@link WLJspc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link WLJspc}
   */
  @Test
  public void testNewWLJspc() {
    // Arrange and Act
    WLJspc actualWlJspc = new WLJspc();

    // Assert
    Location location = actualWlJspc.getLocation();
    assertNull(location.getFileName());
    assertNull(actualWlJspc.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualWlJspc.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualWlJspc.getTaskName());
    assertNull(actualWlJspc.getTaskType());
    assertNull(actualWlJspc.getProject());
    assertNull(actualWlJspc.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualWlJspc.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualWlJspc, runtimeConfigurableWrapper.getProxy());
  }
}
