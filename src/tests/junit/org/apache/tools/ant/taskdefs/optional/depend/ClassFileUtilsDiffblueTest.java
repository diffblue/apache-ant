package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ClassFileUtilsDiffblueTest {
  /**
   * Test {@link ClassFileUtils#convertSlashName(String)}.
   * <p>
   * Method under test: {@link ClassFileUtils#convertSlashName(String)}
   */
  @Test
  public void testConvertSlashName() {
    // Arrange, Act and Assert
    assertEquals("Name", ClassFileUtils.convertSlashName("Name"));
  }

  /**
   * Test {@link ClassFileUtils#convertDotName(String)}.
   * <p>
   * Method under test: {@link ClassFileUtils#convertDotName(String)}
   */
  @Test
  public void testConvertDotName() {
    // Arrange, Act and Assert
    assertEquals("Dot Name", ClassFileUtils.convertDotName("Dot Name"));
  }
}
