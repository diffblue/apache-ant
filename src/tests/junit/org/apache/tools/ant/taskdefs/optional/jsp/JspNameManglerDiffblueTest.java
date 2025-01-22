package org.apache.tools.ant.taskdefs.optional.jsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class JspNameManglerDiffblueTest {
  /**
   * Test {@link JspNameMangler#mapJspToJavaName(File)}.
   * <ul>
   *   <li>Then return {@code assert_00025.java}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspNameMangler#mapJspToJavaName(File)}
   */
  @Test
  public void testMapJspToJavaName_thenReturnAssert00025Java() {
    // Arrange
    JspNameMangler jspNameMangler = new JspNameMangler();

    // Act and Assert
    assertEquals("assert_00025.java",
        jspNameMangler.mapJspToJavaName(Paths.get(System.getProperty("java.io.tmpdir"), "assert").toFile()));
  }

  /**
   * Test {@link JspNameMangler#mapJspToJavaName(File)}.
   * <ul>
   *   <li>Then return {@code test_0002etxt.java}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspNameMangler#mapJspToJavaName(File)}
   */
  @Test
  public void testMapJspToJavaName_thenReturnTest0002etxtJava() {
    // Arrange
    JspNameMangler jspNameMangler = new JspNameMangler();

    // Act and Assert
    assertEquals("test_0002etxt.java",
        jspNameMangler.mapJspToJavaName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link JspNameMangler#mapJspToJavaName(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} toFile.</li>
   *   <li>Then return {@code _000342.java}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspNameMangler#mapJspToJavaName(File)}
   */
  @Test
  public void testMapJspToJavaName_whenPropertyIsJavaIoTmpdirIs42ToFile_thenReturn000342Java() {
    // Arrange
    JspNameMangler jspNameMangler = new JspNameMangler();

    // Act and Assert
    assertEquals("_000342.java",
        jspNameMangler.mapJspToJavaName(Paths.get(System.getProperty("java.io.tmpdir"), "42").toFile()));
  }

  /**
   * Test {@link JspNameMangler#mapPath(String)}.
   * <p>
   * Method under test: {@link JspNameMangler#mapPath(String)}
   */
  @Test
  public void testMapPath() {
    // Arrange, Act and Assert
    assertNull((new JspNameMangler()).mapPath("Path"));
  }
}
