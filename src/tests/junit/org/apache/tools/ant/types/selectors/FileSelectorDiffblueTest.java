package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertFalse;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.BZip2Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class FileSelectorDiffblueTest {
  /**
   * Test {@link FileSelector#isSelected(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenScriptSelector_whenResource_thenReturnFalse() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();

    // Act and Assert
    assertFalse(scriptSelector.isSelected(new Resource()));
  }

  /**
   * Test {@link FileSelector#isSelected(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenBZip2Resource_thenReturnFalse() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    scriptSelector.addText("Text");

    // Act and Assert
    assertFalse(scriptSelector.isSelected(new BZip2Resource()));
  }

  /**
   * Test {@link FileSelector#isSelected(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenFileResource_thenReturnFalse() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    scriptSelector.addText("Text");

    // Act and Assert
    assertFalse(scriptSelector.isSelected(new FileResource()));
  }
}
