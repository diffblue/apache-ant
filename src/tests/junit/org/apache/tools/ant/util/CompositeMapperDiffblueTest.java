package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CompositeMapperDiffblueTest {
  /**
   * Test {@link CompositeMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link CompositeMapper} (default constructor) addConfigured {@link CompositeMapper} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenCompositeMapperAddConfiguredCompositeMapper_thenReturnNull() {
    // Arrange
    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new CompositeMapper());

    // Act and Assert
    assertNull(compositeMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link CompositeMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link CompositeMapper} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenCompositeMapper_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new CompositeMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link CompositeMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenReturnArrayOfStringWithFooTxt() {
    // Arrange
    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new ChainedMapper());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, compositeMapper.mapFileName("foo.txt"));
  }

  /**
   * Test new {@link CompositeMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CompositeMapper}
   */
  @Test
  public void testNewCompositeMapper() {
    // Arrange, Act and Assert
    assertTrue((new CompositeMapper()).getMappers().isEmpty());
  }
}
