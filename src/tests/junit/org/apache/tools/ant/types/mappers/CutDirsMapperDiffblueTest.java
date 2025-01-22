package org.apache.tools.ant.types.mappers;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CutDirsMapperDiffblueTest {
  /**
   * Test {@link CutDirsMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor) Dirs is one.</li>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CutDirsMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenCutDirsMapperDirsIsOne_whenFooTxt_thenReturnNull() {
    // Arrange
    CutDirsMapper cutDirsMapper = new CutDirsMapper();
    cutDirsMapper.setDirs(1);

    // Act and Assert
    assertNull(cutDirsMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link CutDirsMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor) Dirs is one.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CutDirsMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenCutDirsMapperDirsIsOne_whenNull_thenReturnNull() {
    // Arrange
    CutDirsMapper cutDirsMapper = new CutDirsMapper();
    cutDirsMapper.setDirs(1);

    // Act and Assert
    assertNull(cutDirsMapper.mapFileName(null));
  }

  /**
   * Test {@link CutDirsMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor).</li>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CutDirsMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenCutDirsMapper_whenFooTxt_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CutDirsMapper()).mapFileName("foo.txt"));
  }
}
