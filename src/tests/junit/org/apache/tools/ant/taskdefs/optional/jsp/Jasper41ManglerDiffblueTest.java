package org.apache.tools.ant.taskdefs.optional.jsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class Jasper41ManglerDiffblueTest {
  /**
   * Test {@link Jasper41Mangler#mapJspToJavaName(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} toFile.</li>
   *   <li>Then return {@code _42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jasper41Mangler#mapJspToJavaName(File)}
   */
  @Test
  public void testMapJspToJavaName_whenPropertyIsJavaIoTmpdirIs42ToFile_thenReturn42() {
    // Arrange
    Jasper41Mangler jasper41Mangler = new Jasper41Mangler();

    // Act and Assert
    assertEquals("_42",
        jasper41Mangler.mapJspToJavaName(Paths.get(System.getProperty("java.io.tmpdir"), "42").toFile()));
  }

  /**
   * Test {@link Jasper41Mangler#mapJspToJavaName(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code test_txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jasper41Mangler#mapJspToJavaName(File)}
   */
  @Test
  public void testMapJspToJavaName_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnTestTxt() {
    // Arrange
    Jasper41Mangler jasper41Mangler = new Jasper41Mangler();

    // Act and Assert
    assertEquals("test_txt",
        jasper41Mangler.mapJspToJavaName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link Jasper41Mangler#mapPath(String)}.
   * <p>
   * Method under test: {@link Jasper41Mangler#mapPath(String)}
   */
  @Test
  public void testMapPath() {
    // Arrange, Act and Assert
    assertNull((new Jasper41Mangler()).mapPath("Path"));
  }
}
